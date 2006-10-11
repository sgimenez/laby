exception Invalid
exception Conflict
exception Error

type action =
    | Noop
    | Excl of (unit -> unit)
    | Do of (unit -> unit)

type handle = unit -> action
type handle_s = string -> action
type t = char * string * handle option * handle_s option * F.t
val noshort : char
val nolong : string

val cmd : t list -> (string list -> unit) -> unit

val debug_opt : t
val log_opt : default:(string option) -> t
val theme_opt : default:(string option) -> t
