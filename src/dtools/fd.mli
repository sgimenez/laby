(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

(**
   Formated display management module.
*)

val tag : string -> F.t -> F.t
val def_text : (string * string) -> string -> unit

val render_raw : F.t -> string
val render_color : F.t -> string

val stdout : F.t -> unit
val stderr : F.t -> unit

val output : F.t -> unit
val input : unit -> string option

val exn : exn -> unit

(**/*)
val conf_tags : Conf.ut
