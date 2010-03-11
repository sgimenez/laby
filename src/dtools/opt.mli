(*
   Copyright (C) 2007-2010 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

type action =
  | Undefined
  | Invalid of F.t
  | Error of F.t
  | Excl of (unit -> F.t)
  | Do of (unit -> unit)

type spec =
  [
  | `None
  | `Any of string option -> action
  | `Arg of string -> action
  | `Do_unit of unit -> unit
  | `Excl_unit of unit -> F.t
  | `Do_string of string -> unit
  | `Excl_string of string -> F.t
  | `Do_int of int -> unit
  | `Excl_int of int -> F.t
  | `Do_optint of int option -> unit
  | `Excl_optint of int option -> F.t
  ]

type t

val make : ?short:char -> ?long:string -> spec -> F.t -> t

val cmd : ?argv:(string array) -> t list ->
  [
  | `Errors of F.t list
  | `Excl of (unit -> F.t)
  | `Proceed of string list
  ]

val conf : ?short:char -> ?long:string -> 'a Conf.t -> t
  (**
     Generate an option to configure a given configuration key.
  *)

val conf_set : ?short:char -> ?long:string -> Conf.ut -> t
  (**
     Generate an general option to configure sub configuration keys.
  *)

val conf_descr : ?short:char -> ?long:string -> Conf.ut -> t
val conf_dump : ?short:char -> ?long:string -> Conf.ut -> t
  (**
     Generate an option to describe/dump a Conf.ut configuration key.
  *)
