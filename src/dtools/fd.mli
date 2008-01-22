
val tag : string -> F.t -> F.t -> F.t
val load_theme : string -> unit

val string : F.t -> string
val stdout : F.t -> unit

val load_texts : string -> unit

val output : F.t -> unit
val input : unit -> string option
val exn : exn -> unit
