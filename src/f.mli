type t
(** atom *)
type l = unit -> t
(** flow *)

val n : t
(** null atom *)
val f : ?ind:t -> t list -> t
(** flowed box *)
val h : ?sep:t -> t list -> t
(** horizontal box *)
val v : ?head:t -> t list -> t
(** vertical box *)
val p : int -> ?delim:(t * t) -> t -> t
(** precedence box *)
val pn : int -> t -> t
(** precedence unbox *)
val t : string -> t -> t
(** tag box *)

val text : string -> (string * t) list -> t
(** text box *)

val s : string -> t
(** text atom *)
val sb : ?padding:int -> string -> t
(** string atom *)
val sq : string -> t
(** quoted string atom *)
val i : ?zeros:int -> int -> t
(** integer atom, with specified zero padding *)
val l : string -> t list -> t
(** label atom *)

val set_debug : int option -> unit
val log : string -> unit

val set_theme : string -> unit
val set_texts : string -> unit

val print : ?l:string -> ?d:int -> ?e:int -> (unit -> t) -> unit
val string : t -> string
val prompt : (unit -> t) -> string option

val exn : exn -> unit
