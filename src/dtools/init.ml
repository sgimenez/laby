
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

let conf =
  Conf.void
    (F.x "initialization configuration" [])
let conf_daemon =
  Conf.bool ~p:(conf#plug "daemon") ~d:false
    (F.x "run in daemon mode" [])
let conf_daemon_pidfile =
  Conf.bool ~p:(conf_daemon#plug "pidfile") ~d:false
    (F.x "support for pidfile generation" [])
let conf_daemon_pidfile_path =
  Conf.string ~p:(conf_daemon_pidfile#plug "path")
    (F.x "path to pidfile" [])
let conf_trace =
  Conf.bool ~p:(conf#plug "trace") ~d:false
    (F.x "dump an initialization trace" [])
let conf_concurrent =
  Conf.bool ~p:(conf#plug "concurrent") ~d:false
    (F.x "run initialization using concurrent threads" [])
let conf_catch_exn =
  Conf.bool ~p:(conf#plug "catch_exn") ~d:false
    (F.x "catch exceptions, use false to backtrace exceptions" [])

type t =
    {
      name: string;
      mutable launched: bool;
      mutable depends: t list;
      mutable triggers: t list;
      mutable mutex: T.mutex;
      f: unit -> unit;
    }

let make ?(name="") ?(depends=[]) ?(triggers=[]) ?(after=[]) ?(before=[]) f =
  let na =
    {
      name = name;
      launched = false;
      depends = depends;
      triggers = triggers;
      mutex = T.mutex ();
      f = f;
    }
  in
  List.iter (fun a -> a.triggers <- na :: a.triggers) after;
  List.iter (fun a -> a.depends <- na :: a.depends) before;
  na

let start = make ~name:"init-start" flush_all

let stop = make ~name:"init-stop" flush_all

let at_start ?name ?depends ?triggers ?after ?before f =
  let a = make ?name ?depends ?triggers ?after ?before f in
  start.triggers <- a :: start.triggers;
  a

let at_stop ?name ?depends ?triggers ?after ?before f =
  let a = make ?name ?depends ?triggers ?after ?before f in
  stop.depends <- a :: stop.depends;
  a

let rec exec a =
  let log =
    if conf_trace#get then
      begin fun s ->
	let id = (*Thread.id (Thread.self ()) *) 0 in
	Printf.printf "init(%i):%-35s@%s\n%!" id a.name s
      end
    else
      begin fun s -> () end
  in
  let rec exec a =
    log "called";
    a.mutex#lock;
    begin try
	if not a.launched
	then begin
          a.launched <- true;
	  log "start";
	  log "start-depends";
	  mult_exec a.depends;
	  log "stop-depends";
	  log "start-atom";
	  a.f ();
	  log "stop-atom";
	  log "start-triggers";
	  mult_exec a.triggers;
	  log "stop-triggers";
	  log "stop";
	end;
      a.mutex#unlock;
      log "return"
      with
      | e -> a.mutex#unlock; raise e
    end
  and mult_exec l =
    begin match conf_concurrent#get with
    | true ->
	let ask x =
	  log (Printf.sprintf "exec %s" x.name);
	  fun () -> exec x
	in
	T.exec (List.map ask l)
    | false ->
	List.iter exec l
    end
  in
  exec a

exception StartError of exn
exception StopError of exn

let exit i =
  exit i

let main f () =
  begin try exec start with e -> raise (StartError e) end;
  begin try f () with
    | e ->
	let se = Printexc.to_string e in
	Printf.eprintf
	  "init: exception encountered during main phase:\n  %s\n%!" se;
	Printf.eprintf "exception: %s\n%!" se;
	if conf_catch_exn#get then raise e
  end;
  begin try exec stop with e -> raise (StopError e) end

let catch f clean =
  begin try
      f (); clean ()
    with
    | StartError (e) ->
	Printf.eprintf
	  "init: exception encountered during start phase:\n  %s\n%!"
	  (Printexc.to_string e);
	clean ();
	exit (-1)
    | StopError (e) ->
	Printf.eprintf
	  "init: exception encountered during stop phase:\n  %s\n%!"
	  (Printexc.to_string e);
	clean ();
	exit (-1)
  end

let reopen fd filename =
  let opts = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
  let fd2 = Unix.openfile filename opts 0o666 in
  Unix.dup2 fd2 fd;
  Unix.close fd2

let daemonize fn =
  flush_all ();
  reopen Unix.stdin "/dev/null";
  reopen Unix.stdout "/dev/null";
  reopen Unix.stderr "/dev/null";
  begin match Unix.fork () with
  | 0 ->
      if (Unix.setsid () < 0) then exit 1;
      begin match conf_daemon_pidfile#get with
      | false ->
	  catch fn (fun () -> ())
      | true ->
	  let filename = conf_daemon_pidfile_path#get in
	  let f = open_out filename in
	  let pid = Unix.getpid () in
	  output_string f (string_of_int pid);
	  output_char f '\n';
	  close_out f;
	  catch fn (fun () -> Unix.unlink filename)
      end;
      exit 0
  | _ -> exit 0
  end

let exit_when_root () =
  let security s = Printf.eprintf "init: security exit, %s\n%!" s in
  if Unix.geteuid () = 0 then
    begin security "root euid."; exit (-1) end;
  if Unix.getegid () = 0 then
    begin security "root egid."; exit (-1) end

let init ?(prohibit_root=false) f =
  if prohibit_root then exit_when_root ();
  let signal_h i = () in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle signal_h);
  Sys.set_signal Sys.sigint (Sys.Signal_handle signal_h);
  if conf_daemon#get
  then daemonize (main f)
  else catch (main f) (fun () -> ())

let opt_daemon =
  Opt.make ~short:'d' ~long:"daemon"
    ~noarg:(fun () -> Opt.Do (fun () -> conf_daemon#set true))
    (F.x "run in daemon mode" [])

