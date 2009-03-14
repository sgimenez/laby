val conf : Conf.ut
val conf_paths : string list Conf.t

exception Error of F.t

type t = string list

val get : t -> string

val get_list : ?ext:string -> t -> string list

val use : t -> (string -> in_channel -> unit) -> unit
