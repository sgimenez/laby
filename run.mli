exception Signal of int

val catch : (exn -> unit) option ref

type 'a result =
    | Done of 'a
    | Exited
    | Failed
    | Exn of exn

val exec : (unit -> 'a) -> 'a
val run : (unit -> 'a) -> 'a result

val hook : ('a -> 'b) -> 'a -> (unit -> unit) -> 'b

val timeout : ?seconds:int -> ('a -> 'b) -> 'a -> (unit -> 'b) -> 'b

val exit : unit -> 'a
val fail : unit -> 'a
