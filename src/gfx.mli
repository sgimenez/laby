exception Error
val display_gtk : string ->
  (unit -> (unit -> string option) * (string -> unit) * (unit -> unit))
  -> unit
