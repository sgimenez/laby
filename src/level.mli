type t

val basic : t
val load : string -> t

val generate : t -> State.t
