(** format *)

type t

(** formaters *)

val n : t
(** null box *)
val t : string -> t -> t
(** tag box *)

val f : ?ind:t -> t list -> t
(** flowed box *)
val h : ?sep:t -> t list -> t
(** horizontal box *)
val v : ?head:t -> t list -> t
(** vertical box *)
val l : string -> t -> t
(** label box *)

val p : int -> ?delim:(t * t) -> t -> t
(** precedence box *)
val pn : int -> t -> t
(** precedence unboxed *)

val q : t -> t
(** quote box *)

val x : string -> (string * t) list -> t
(** text box *)

(** atoms *)

val s : string -> t
(** string atom *)
val sb : ?padding:int -> string -> t
(** boxed string atom *)
val sq : string -> t
(** quoted string atom *)
val i : ?zeros:int -> int -> t
(** integer atom, with specified zero padding *)
val float : ?zeros:int -> float -> t
(** float atom, with specified zero padding *)

(** settings *)

val set_debug : int option -> unit
val log : string -> unit

val set_theme : string -> unit
val set_texts : string -> unit

(** display *)

val print : ?l:string -> ?d:int -> ?e:int -> t -> unit
val string : t -> string
val prompt : t -> string option

val exn : exn -> unit
