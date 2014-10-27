
(*
 * Copyright (C) 2007-2014 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

type tile = [ `Void | `Wall | `Exit | `Rock | `Web | `NRock | `NWeb ]
type dir = [ `N | `E | `S | `W ]

type action =
 [ `None
 | `Wall_In | `Rock_In | `Exit_In | `Web_In
 | `Web_Out
 | `Exit | `No_Exit | `Carry_Exit
 | `Rock_Take | `Rock_Drop
 | `Take_Nothing | `Take_No_Space | `Drop_Nothing | `Drop_No_Space
 | `Say of string
 ]

type t

val make : tile array array -> int * int -> dir -> t
val copy : t -> t

val run : string -> t -> string * t

val iter_map : t -> (int -> int -> tile -> unit) -> unit
val pos : t -> int * int
val carry : t -> [ `None | `Rock ]
val dir : t -> dir
val action : t -> action

val random_walk : t -> t
