(*
   Copyright (C) 2007-2010 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

let conf =
  Conf.void
    (F.x "run configuration" [])
let conf_daemon =
  Conf.bool ~p:(conf#plug "daemon") ~d:false
    (F.x "run in daemon mode" [])
let conf_daemon_pidfile =
  Conf.bool ~p:(conf_daemon#plug "pidfile") ~d:false
    (F.x "support for pidfile generation" [])
let conf_daemon_pidfile_path =
  Conf.string ~p:(conf_daemon_pidfile#plug "path")
    (F.x "path to pidfile" [])

let opt_debug =
  let action i =
    Log.conf_level#set (match i with None -> 5 | Some x -> x)
  in
  Opt.make ~long:"debug" (`Do_optint action)
    (F.x "activate debug mode" [])

let opt_daemon =
  let action () = conf_daemon#set true in
  Opt.make ~short:'d' ~long:"daemon" (`Do_unit action)
    (F.x "run in daemon mode" [])


exception Signal of int

let signal i =
  Log.master#debug 1 (
    F.x "received signal <number>" [
      "number", F.int i;
    ]
  );
  raise (Signal i)

exception Fail of F.t

let reopen fd filename =
  let opts = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
  let fd2 = Unix.openfile filename opts 0o666 in
  Unix.dup2 fd2 fd;
  Unix.close fd2

let daemonize () =
  flush_all ();
  reopen Unix.stdin "/dev/null";
  reopen Unix.stdout "/dev/null";
  reopen Unix.stderr "/dev/null";
  begin match Unix.fork () with
  | 0 ->
      if (Unix.setsid () < 0) then exit 1;
      begin match conf_daemon_pidfile#get with
      | false ->
	  fun () -> ()
      | true ->
	  let filename = conf_daemon_pidfile_path#get in
	  let f = open_out filename in
	  let pid = Unix.getpid () in
	  output_string f (string_of_int pid);
	  output_char f '\n';
	  close_out f;
	  fun () -> Unix.unlink filename
      end
  | _ -> exit 0
  end

let exit_when_root () =
  let security s =
    Log.master#fatal (
      F.x "security exit: <msg>" [
	"msg", F.string s;
      ]
    );
    exit (-2)
  in
  if Unix.geteuid () = 0 then security "root euid";
  if Unix.getegid () = 0 then security "root egid"

let init ?(prohibit_root=false) ?name ?conf ?services action =
  if prohibit_root then exit_when_root ();
  Res.conf_domain#set_d name;
  begin match conf with
  | None -> ()
  | Some (conf, res) ->
      begin try
	Conf.load ~log:Log.master#warning conf (Res.get res)
      with
      | Res.Error msg ->
	  Log.master#fatal msg;
	  exit 3
      end
  end;
  let f =
    begin match action with
    | `Main f -> f
    | `Opts (opts, proceed) ->
	begin match Opt.cmd opts with
	| `Errors ml ->
	    begin fun () ->
	      Log.master#error (
		F.x "invalid options: <errors>" [
		  "errors", F.v ml;
		]
	      );
	      exit 1
	    end
	| `Excl fn ->
	    (fun () -> Fd.stdout (fn ()))
	| `Proceed list ->
	    (fun () -> proceed list)
	end
    end
  in
  Sys.catch_break true;
  begin match Sys.os_type with
  | "Unix" ->
      Sys.set_signal Sys.sigpipe (Sys.Signal_handle signal);
      Sys.set_signal Sys.sigquit (Sys.Signal_handle signal);
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle signal);
      Sys.set_signal Sys.sighup Sys.Signal_ignore;
  | _ -> ()
  end;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle signal);
  let clean =
    begin match conf_daemon#get with
    | false -> fun () -> ()
    | true -> daemonize ()
    end
  in
  begin try
    Srv.launch ?services f
  with
  | Fail msg ->
      clean ();
      exit 2
  | Res.Error msg ->
      Log.master#fatal msg;
      clean ();
      exit 3
  | Signal i when i = Sys.sigterm || i = Sys.sigquit -> ()
  | Sys.Break -> ()
  | Srv.StartError (e) ->
      Log.master#internal (
	F.x "exception encountered during start phase: <exn>"
	  ["exn", F.v [F.exn e]]
      );
      clean ();
      raise e
  | Srv.StopError (e) ->
      Log.master#internal (
	F.x "exception encountered during stop phase: <exn>"
	  ["exn", F.v [F.exn e]]
      );
      clean ();
      raise e
  | e ->
      Log.master#internal (
	F.x "exception: <exn>"
	  ["exn", F.v [F.exn e]]
      );
      clean ();
      raise e
  end;
  clean ()

let fatal msg =
  Log.master#fatal msg;
  raise (Fail msg)

type 'a result =
    | Done of 'a
    | Failed of F.t
    | Exn of exn

let exec f =
  begin try Done (f ()) with
  | (Sys.Break | Signal _) as exn -> raise exn
  | Fail msg -> Failed msg
  | exn -> Exn exn
  end

let hook f a h =
  begin try let r = f a in h (); r with
  | exn -> h (); raise exn
  end

let timeout ?(seconds=2) f a h =
  begin match Sys.os_type with
  | "Unix" | "Cygwin" ->
      ignore (Unix.alarm seconds);
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle signal);
      let start () = (try f a with Signal i when i = Sys.sigalrm -> h ()) in
      hook start () (fun () ->
        Sys.set_signal Sys.sigalrm Sys.Signal_ignore
      )
  | _ -> f a
  end

