(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

(**
   Formated message management module.
*)

type t
(** Type of formated messages *)


(** Boxes *)

val n : t
(** null box *)

val h : ?sep:t -> t list -> t
(** horizontal box *)

val v : ?head:t -> t list -> t
(** vertical box *)

val l : string -> t -> t
(** label box *)

val p : int -> ?wrap:(t -> t) -> t -> t
(** precedence box *)

val q : t -> t
(** quote box *)

val s : string -> t
(** string box *)

val x : string -> (string * t) list -> t
(** localized text box *)


(** Atomic type boxes *)

val string : string -> t
(** string atom *)
val int : int -> t
(** integer atom, with specified zero padding *)
val float : float -> t
(** float atom, with specified zero padding *)
val exn : exn -> t
(** exn atom *)
val time : float -> t
(** timestamp atom *)


(** Taging *)

type tag = string -> string

val t : tag -> t -> t
(** taged message *)


(** Lazyness *)

val z : (unit -> t) -> t
(** lazy generation *)


(**/**)

val use : t ->
  [ `N
  | `T of tag * t
  | `S of string
  | `L of string * t
  | `H of t * t list
  | `V of t * t list
  | `Q of t
  | `P of int * (t -> t) * t
  | `X of string * (string * t) list
  | `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Exn of exn
  | `Time of float
  ]

