(*
   Copyright (C) 2007-2010 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

(**
   Service management module.
   Allow to define procedures that must be executed at start up, and
   procedures that are to be executed at exit to have a clean quit.
*)

type t

val start : t
  (** Root start atom *)
val stop : t
  (** Root stop atom *)

val make :
  ?name:string ->
  ?depends:(t list) -> ?triggers:(t list) ->
  ?after:(t list) -> ?before:(t list) ->
  (unit -> unit) -> t
  (**
     Define a init atom associated with the given [(unit -> unit)]
     procedure, which eventualy depends on others atoms (these atoms
     will be executed before the one currently defined) an triggers
     other atoms (these atoms will be executed after the one currently
     defined). [after] and [before] allow to register the currently
     defined atom in the depend and triggers lists of other atoms.
  *)

val at_start :
  ?name:string ->
  ?depends:(t list) -> ?triggers:(t list) ->
  ?after:(t list) -> ?before:(t list) ->
  (unit -> unit) -> t
  (**
     Same as [make] plus a shortcut for "after Srv.start".
  *)

val at_stop :
  ?name:string ->
  ?depends:(t list) -> ?triggers:(t list) ->
  ?after:(t list) -> ?before:(t list) ->
  (unit -> unit) -> t
  (**
     Same as [make] plus a shortcut for "before Srv.stop".
  *)

val exec : t -> unit
  (**
     Launch the execution of a given init atom.
  *)

val launch :
  ?services:t list ->
  (unit -> unit) -> unit
  (**
     It first execute the registered start atoms, then call
     the main procedure, then execute the registered stop atoms.
  *)

exception StartError of exn
exception StopError of exn

val conf : Conf.ut
val conf_concurrent : bool Conf.t
val conf_trace : bool Conf.t

