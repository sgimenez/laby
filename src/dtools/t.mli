type mutex = < lock: unit; unlock: unit; >

val mutex : unit -> mutex

val exec : (unit -> unit) list -> unit

val mutex_ref : (unit -> mutex) ref
val exec_ref : ((unit -> unit) list -> unit) ref
