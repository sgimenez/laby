
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let start =
  F.x "I'm ready." []

let wall_in =
  F.x "I can't go through the wall." []

let rock_in =
  F.x "I can't go through this rock." []

let exit_in =
  F.x "I can't go through the door." []

let web_in =
  F.x "Oops, a spider web." []

let web_out =
  F.x "I can't move anymore." []

let exit =
  F.x "Wohoo, the exit!" []

let no_exit =
  F.x "I can't find a door to open." []

let carry_exit =
  F.x "I can't go out carrying a rock." []

let rock_no_take =
  F.x "There's no rock to take here." []

let rock_no_drop =
  F.x "I can't drop the rock here." []
