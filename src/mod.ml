
(*
 * Copyright (C) 2007-2010 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let log = Log.make ["mod"]

let conf =
  Conf.void
    (F.x "mods configuration" [])

let conf_selected =
  Conf.string ~p:(conf#plug "selected") ~d:""
    (F.x "select a specific programming mod" [])

type query = string * (string -> unit)

type t =
    <
      name: string;
      check: bool;
      set_buf: string -> unit;
      get_buf: string;
      start: (string -> unit) -> bool;
      probe: (string -> unit) -> query option;
      stop: unit;
      help: string -> string;
    >

type h =
    {
      close : unit -> unit;
      in_ch : Unix.file_descr;
      out_ch : Unix.file_descr;
      err_ch : Unix.file_descr;
      buffer : Buffer.t;
      queue : string Queue.t;
    }

let substs =
  [
    "ant", F.xs "ascii" "ant" [];
    "Ant", F.xs "ascii" "Ant" [];
    "left", F.xs "ascii" "left" [];
    "right", F.xs "ascii" "right" [];
    "forward", F.xs "ascii" "forward" [];
    "take", F.xs "ascii" "take" [];
    "drop", F.xs "ascii" "drop" [];
    "escape", F.xs "ascii" "escape" [];
    "look", F.xs "ascii" "look" [];
    "say", F.xs "ascii" "say" [];
    "Void", F.xs "ascii" "Void" [];
    "Wall", F.xs "ascii" "Wall" [];
    "Rock", F.xs "ascii" "Rock" [];
    "Web", F.xs "ascii" "Web" [];
    "Exit", F.xs "ascii" "Exit" [];
    "Unknown", F.xs "ascii" "Unknown" [];
  ]

let ln_regexp =
  Str.regexp "laby_name_[a-zA-Z0-9]*"

let subst : string -> string =
  let repl x =
    let s = Str.matched_string x in
    let ss = String.sub s 10 (String.length s - 10) in
    begin try
      Fd.render_raw (List.assoc ss substs)
    with
    | Not_found -> ss
    end
  in
  Str.global_substitute ln_regexp repl

let bufsize = 16384
let buffer = String.create bufsize
let buf_read fd f =
  begin match Unix.read fd buffer 0 bufsize with
  | 0 -> false
  | i -> f (String.sub buffer 0 i); true
  end

let output fd s =
  let str = s ^ "\n" in
  let len = String.length str in
  ignore (Unix.write fd str 0 len)

let input ?(timeout=0.5) err h =
  let collect s =
    for j = 0 to String.length s - 1; do
      begin match s.[j] with
      | '\r' -> ()
      | '\n' ->
	  Queue.push (Buffer.contents h.buffer) h.queue;
	  Buffer.clear h.buffer
      | c -> Buffer.add_char h.buffer c
      end
    done
  in
  let rec loop () =
    begin try Some (Queue.pop h.queue, output h.out_ch) with
    | Queue.Empty ->
	begin match Sys.os_type with
	| "Win32" -> (* no select... so forget about err_ch for now *)
	    if buf_read h.in_ch collect then loop () else None
	| _ ->
	    let l, _, _ = Unix.select [h.in_ch; h.err_ch] [] [] timeout in
	    begin match l with
	    | [] -> err "...\n"; None
	    | _ ->
		if List.mem h.err_ch l then
		  ignore (buf_read h.err_ch err);
		if List.mem h.in_ch l then
		  if buf_read h.in_ch collect
		  then loop ()
		  else None
		else loop ()
	    end
	end
    end
  in
  begin try loop () with
  | Unix.Unix_error (Unix.EPIPE, _, _) ->
      begin match Sys.os_type with
      | "Win32" -> (* dump all err_ch on failure *)
	  begin try ignore (buf_read h.err_ch err)  with
	  | Unix.Unix_error (Unix.EPIPE, _, _) -> ()
	  end;
      | _ -> ()
      end;
      None
  end

let tab_regexp = Str.regexp "[ \t]+"

let exe =
  begin match Sys.os_type with
  | "Win32" -> ".exe"
  | _ -> ""
  end

let need x =
  Res.get_bin x <> None

let fetch (tmp, lib, prg, err) x =
  let f = open_out (Res.path [tmp; subst x]) in
  output_string f (subst (Res.read_full (Res.path [lib; x])));
  close_out f;
  true

let dump (tmp, lib, prg, err) x =
  let f = open_out (Res.path [tmp; subst x]) in
  output_string f prg;
  close_out f;
  true

let exec (tmp, lib, prg, err) p pl =
  let r, w = Unix.pipe () in
  let pid =
    Unix.chdir tmp;
    Unix.create_process (p ^ exe) (Array.of_list (p :: pl)) Unix.stdin w w
  in
  let status =
    begin match snd (Unix.waitpid [] pid) with
    | Unix.WEXITED 0 ->
	true
    | Unix.WEXITED i ->
	log#debug 1 (
	  F.x "exection of <command> returned: <errno>" [
	    "errno", F.int i;
	    "command", F.string p;
	  ]);
	false
    | _ ->
	log#debug 1 (
	  F.x "execution of <command> failed" [
	    "command", F.string p;
	  ]
	);
	false
    end
  in
  Unix.close w;
  begin try ignore(buf_read r err) with
  | Unix.Unix_error (Unix.EPIPE, _, _) -> ()
  end;
  Unix.close r;
  status

let make name : t =
  let rules = Res.read_blocks (Res.get ["mods"; name; "lib"; "rules"]) in
  let skel = Res.read_full (Res.get ["mods"; name; "skel"]) in
  let find_tab s b =
    begin match rules s with
    | None -> []
    | Some l -> List.map (Str.split tab_regexp) l
    end
  in
  let info = find_tab "info:" rules in
  let steps = find_tab "run:" rules in
  let fail l =
    Run.fatal (
      F.x "invalid directive: <line>" [
	"line", F.string (String.concat " " l);
      ]
    )
  in
  let check_one l =
    begin match l with
    | ["need"; x] -> need x
    | _ -> fail l
    end
  in
  let rec run a l =
    begin match l with
    | ["fetch"; x] :: q -> if fetch a x then run a q else None
    | ["dump"; x] :: q -> if dump a x then run a q else None
    | ("exec" :: p :: pl) :: q -> if exec a p pl then run a q else None
    | ["spawn" :: p :: pl] -> Some (false, p, pl)
    | ["spawnl" :: p :: pl] -> Some (true, p, pl)
    | [] -> fail ["spawn"]
    | _ -> fail (List.hd l)
    end
  in
  let check = List.for_all check_one info in
  object (self)

    val hr = ref None
    val buffer = ref (subst skel)

    method name = name

    method check = check

    method get_buf = !buffer

    method set_buf buf = buffer := buf

    method start err =
      self#stop;
      let lib = Res.get ["mods"; name; "lib"] in
      let prg = self#get_buf in
      let tmp = Res.mktempdir "ant" in
      begin match run (tmp, lib, prg, err) steps with
      | None -> false
      | Some (local, p, pl) ->
	  let bin =
	    begin match local with
	    | true ->
		let path = Res.path [tmp; p] in
		begin match Sys.os_type with
		| "Win32" -> Unix.rename path (path ^ exe); path ^ exe
		| _ -> path
		end
	    | false -> p ^ exe
	    end
	  in
	  let out_ch', out_ch = Unix.pipe () in
	  let in_ch, in_ch' = Unix.pipe () in
	  let err_ch, err_ch' = Unix.pipe () in
	  let pid =
	    Unix.chdir tmp;
	    let args = Array.of_list (p :: pl) in
	    Unix.create_process bin args out_ch' in_ch' err_ch'
	  in
	  Unix.close in_ch'; Unix.close out_ch'; Unix.close err_ch';
	  let close () =
	    begin match Sys.os_type with
	    | "Unix" | "Cygwin" ->
		Unix.kill pid Sys.sigterm;
		ignore (Unix.waitpid [] pid)
	    | "Win32" -> (* no kill signal *)
		begin try output out_ch "quit" with
		| Unix.Unix_error (_, _, _) -> ()
		end;
		(* either the process kindly terminates or we'll be stuck *)
		ignore (Unix.waitpid [] pid)
	    | _ -> assert false
	    end;
	    Unix.close in_ch; Unix.close out_ch; Unix.close err_ch;
	    Res.rmtempdir tmp;
	  in
	  let h =
	    {
	      close = close;
	      in_ch = in_ch; out_ch = out_ch; err_ch = err_ch;
	      buffer = Buffer.create 64; queue = Queue.create ();
	    }
	  in
	  begin match input ~timeout:5. err h with
	  | Some ("start", output) ->
	      output "go";
	      hr := Some h;
	      true
	  | _ ->
	      h.close ();
	      false
	  end
      end

    method stop =
      begin match !hr with
      | None -> ()
      | Some h -> h.close (); hr := None
      end

    method probe err =
      begin match !hr with
      | None -> None
      | Some h -> input err h
      end

    method help s =
      let sections = Res.read_blocks (Res.get ["mods"; name; "help"]) in
      begin match sections (s ^ ":") with
      | None -> ""
      | Some l -> subst (String.concat "\n" l) ^ "\n"
      end

  end

let pool () =
  begin match conf_selected#get with
  | "" -> List.map make (Res.get_list ["mods"])
  | name -> [make name]
  end

