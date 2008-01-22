type action =
    | Invalid of F.t
    | Error of F.t
    | Noop
    | Excl of (unit -> F.t)
    | Do of (unit -> unit)

type t

val make :
  ?short:char -> ?long:string ->
  ?noarg:(unit -> action) ->
  ?arg:(string -> action) ->
  F.t ->
  t

val cmd : ?argv:(string array) -> t list ->
  [
  | `Errors of F.t list
  | `Excl of (unit -> F.t)
  | `Proceed of string list
  ]

val conf : ?short:char -> ?long:string -> Conf.ut -> t
  (**
     Generate an option to configure a Conf.ut configuration key.
  *)

val conf_descr : ?short:char -> ?long:string -> Conf.ut -> t
  (**
     Generate an option to describe a Conf.ut configuration key.
  *)
