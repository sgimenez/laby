type t

val basic : t
val load : string -> t
val size : t -> int * int

val generate : t -> State.t
