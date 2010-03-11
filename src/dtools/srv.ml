(*
   Copyright (C) 2007-2010 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

let conf =
  Conf.void
    (F.x "initialization configuration" [])
let conf_trace =
  Conf.bool ~p:(conf#plug "trace") ~d:false
    (F.x "dump an initialization trace" [])
let conf_concurrent =
  Conf.bool ~p:(conf#plug "concurrent") ~d:false
    (F.x "run initialization using concurrent threads" [])

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

let main f () =
  begin try exec start with e -> raise (StartError e) end;
  f ();
  begin try exec stop with e -> raise (StopError e) end

let launch ?(services=[]) f =
  start.triggers <- start.triggers @ services;
  main f ()

