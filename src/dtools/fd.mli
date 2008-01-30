(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

(**
   Formated display management module.
*)

val conf : Conf.ut
val init : 'a F.logger -> string -> unit

val tag : string -> F.t -> F.t

val string : F.t -> string
val stdout : F.t -> unit

val output : F.t -> unit
val input : unit -> string option

val exn : exn -> unit

