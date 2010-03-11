(*
   Copyright (C) 2007-2010 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

val conf : Conf.ut

val conf_daemon : bool Conf.t
val conf_daemon_pidfile : bool Conf.t
val conf_daemon_pidfile_path : string Conf.t

val opt_debug : Opt.t
  (** A option for debug mode *)

val opt_daemon : Opt.t
  (** A option for daemon mode *)

val init :
  ?prohibit_root:bool ->
  ?path:string list ->
  ?conf:(Conf.ut * Res.t) ->
  ?services:Srv.t list ->
  [
  | `Main of unit -> unit
  | `Opts of Opt.t list * (string list -> unit)
  ]
  -> unit
  (**
     This fuction must be used to launch the main procedure of the
     program.
     When invoqued with [~prohibit_root:true], it checks for root access
     rights (euid, egid) and exit in this case.
  *)

exception Signal of int

type 'a result =
    | Done of 'a
    | Failed of F.t
    | Exn of exn

val exec : (unit -> 'a) -> 'a result

val hook : ('a -> 'b) -> 'a -> (unit -> unit) -> 'b

val timeout : ?seconds:int -> ('a -> 'b) -> 'a -> (unit -> 'b) -> 'b

val fatal : F.t -> 'a

