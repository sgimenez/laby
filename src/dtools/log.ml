(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

type t =
    <
      f: 'a. int -> ('a, unit, string, unit) format4 -> 'a;
      internal: F.t -> unit;
      fatal: F.t -> unit;
      error: F.t -> unit;
      warning: F.t -> unit;
      info: F.t -> unit;
      debug: int -> F.t -> unit;
    >

let conf =
  Conf.void
    (F.x "log configuration" [])

let conf_level =
  Conf.int ~p:(conf#plug "level") ~d:3
    (F.x "general log level" [])
let conf_timestamps =
  Conf.bool ~p:(conf#plug "timestamps") ~d:false
    (F.x "display timestamps" [])
let conf_timestamps_format =
  Conf.string ~p:(conf_timestamps#plug "format") ~d:"localized"
    ~comments:[
      F.x "\"localized\" for human readable timestamps in local timezone" [];
      F.x "\"unix\" for subsecond accuracy, and is timezone independant" [];
    ]
    (F.x "display timestamps in unix format" [])

let conf_file  =
  Conf.bool ~p:(conf#plug "file") ~d:false
    (F.x "log to file" [])
let conf_file_path =
  Conf.string ~p:(conf_file#plug "path")
    (F.x "path to log file" [])
let conf_file_append =
  Conf.bool ~p:(conf_file#plug "append") ~d:true
    (F.x "append log to the file" [])
let conf_file_perms =
  Conf.int ~p:(conf_file#plug "perms") ~d:0o600
    (F.x "log file permissions" [])
let conf_stdout =
  Conf.bool ~p:(conf#plug "stdout") ~d:true
    (F.x "log to stdout" [])

let state : [ `Chan of out_channel | `Buffer of F.t list ] ref =
  ref (`Buffer [])

let timestamp time =
  begin match conf_timestamps_format#get with
  | "unix" -> F.float time
  | _ -> F.time time
  end

(* Avoiding interlacing logs *)
let mutexify =
  let log_mutex = Mutex.create () in
  begin fun f x ->
    Mutex.lock log_mutex;
    begin try f x; Mutex.unlock log_mutex with
    | e -> Mutex.unlock log_mutex; raise e
    end
  end

let to_ch ch x =
  Printf.fprintf ch "%s\n%!" (Fd.string x)

let to_stdout x =
  Printf.printf "%s\n%!" (Fd.string x)

let print_stdout x =
  if conf_stdout#get then
    to_stdout x

let print_file x =
  if conf_file#get then
    begin match !state with
    | `Buffer l -> state := `Buffer (x :: l)
    | `Chan ch -> to_ch ch x
    end

let proceed x =
  mutexify (fun () ->
    print_stdout x;
    print_file x;
  ) ()

let build path =
  let rec aux p l (t : Conf.ut) =
    begin match p with
    | [] -> t :: l
    | s :: q ->
	let st =
	  begin try t#path [s] with
	  | Conf.Unbound _ ->
	      let c =
		Conf.int ~p:(t#plug s)
		  (F.x "subordinate log level" [])
	      in
	      c#ut
	  end
	in
	aux q (t :: l) st
    end
  in
  aux path [] conf_level#ut

let tag_internal = F.t "log.internal"
let tag_fatal = F.t "log.fatal"
let tag_error = F.t "log.error"
let tag_warning = F.t "log.warning"
let tag_info = F.t "log.info"
let tag_debug = F.t "log.debug"

let make path : t =
  let confs = build path in
  let path_str = Conf.string_of_path path in
object (self : t)
  val print =
    begin fun tag x ->
      let time = Unix.gettimeofday () in
      let ts = if conf_timestamps#get then [timestamp time] else [] in
      proceed (F.h (ts @ [ tag (F.s "msg:"); F.s path_str; x]))
    end
  val active =
    begin fun lvl ->
      let rec aux l =
	begin match l with
	| [] -> false
	| t :: q ->
	    begin try lvl <= (Conf.as_int t)#get with
	    | Conf.Undefined _ -> aux q
	    end
	end
      in
      aux confs
    end
  method f lvl =
    begin match active lvl with
    | true ->
	Printf.ksprintf (fun s -> print (fun x -> x) (F.s s))
    | false ->
	Printf.ksprintf (fun _ -> ())
    end
  method internal =
    begin match active (-1) with
    | true -> print tag_internal
    | false -> (fun _ -> ())
    end
  method fatal =
    begin match active 0 with
    | true -> print tag_fatal
    | false -> (fun _ -> ())
    end
  method error =
    begin match active 1 with
    | true -> print tag_error
    | false -> (fun _ -> ())
    end
  method warning =
    begin match active 2 with
    | true -> print tag_warning
    | false -> (fun _ -> ())
    end
  method info =
    begin match active 3 with
    | true -> print tag_info
    | false -> (fun _ -> ())
    end
  method debug lvl =
    begin match active lvl with
    | true -> print tag_debug
    | false -> (fun _ -> ())
    end
end

let init () =
  let time = Unix.gettimeofday () in
  let open_log () =
    if conf_file#get then
      begin
	let opts =
	  [Open_wronly; Open_creat; Open_nonblock]
	  @ (if conf_file_append#get then [Open_append] else [Open_trunc])
	in
	let log_file_path = conf_file_path#get in
	let log_file_perms = conf_file_perms#get in
	state := `Chan (open_out_gen opts log_file_perms log_file_path)
      end
  in
  (* Re-open log file on SIGUSR1 -- for logrotate *)
  Sys.set_signal Sys.sigusr1
    (Sys.Signal_handle (fun _ ->
      mutexify begin fun () ->
	begin match !state with
	| `Chan ch -> close_out ch; open_log ()
	| _ -> ()
	end
      end ();
    ));
  mutexify begin fun () ->
    begin match !state with
    | `Buffer l ->
	open_log ();
	begin match !state with
	| `Buffer l -> ()
	| `Chan ch ->
	    to_ch ch (
	      F.x ">>> LOG START <time>" ["time", F.time time]
	    );
	    List.iter (to_ch ch) (List.rev l)
	end
    | _ -> ()
    end;
  end ()

let start = Init.make ~name:"init-log-start" ~before:[Init.start] init

let close () =
  let time = Unix.gettimeofday () in
  begin match !state with
  | `Chan ch ->
      mutexify begin fun () ->
	to_ch ch (
	  F.x ">>> LOG END <time>" ["time", F.time time]
	);
	close_out ch;
	state := `Buffer []
      end ();
  | _ -> ()
  end

let stop = Init.make ~name:"init-log-stop" ~after:[Init.stop] close

let args =
  [
    ["--log-stdout"],
    Arg.Unit (fun () -> conf_stdout#set true),
    "log also to stdout";
    ["--log-file";"-l"],
    Arg.String (fun s -> conf_file#set true; conf_file_path#set s),
    "log file";
  ]
