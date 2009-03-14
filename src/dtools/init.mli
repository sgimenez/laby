(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

(**
   Initialisation management module.
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
     Same as [make] plus a shortcut for "after Init.start".
  *)

val at_stop :
  ?name:string ->
  ?depends:(t list) -> ?triggers:(t list) ->
  ?after:(t list) -> ?before:(t list) ->
  (unit -> unit) -> t
  (**
     Same as [make] plus a shortcut for "before Init.stop".
  *)

val exec : t -> unit
  (**
     Launch the execution of a given init atom.
  *)

val init :
  ?services:t list ->
  (unit -> unit) -> unit
  (**
     This fuction must be used to launch the main procedure of the
     program. It first execute the registered start atoms, then call
     the main procedure, then execute the registered stop atoms.
     Exceptions raised by the main procedure are catched, in order to
     close properly even in such cases. Exceptions are raised again
     after cleaning.
     When invoqued with [~prohibit_root:true], it checks for root access
     rights (euid, egid) and exit in this case.
  *)

exception StartError of exn
exception StopError of exn

val conf : Conf.ut
val conf_concurrent : bool Conf.t
val conf_trace : bool Conf.t
val conf_catch_exn : bool Conf.t


