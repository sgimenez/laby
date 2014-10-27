
(*
 * Copyright (C) 2007-2014 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

val good_start : F.t
val bad_start : F.t

val action : (F.t -> unit) -> State.action -> unit
