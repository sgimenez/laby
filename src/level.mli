
(*
 * Copyright (C) 2007-2010 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

type t
  (* type of levels *)

val load : string -> t
  (* loads a given file *)

val size : t -> int * int
  (* width and height of the level *)

val title : t -> string
val comment : t -> string
val help : t -> string

val generate : t -> State.t
  (* generates a initial labyrinth state for the given level *)

val dummy : t
