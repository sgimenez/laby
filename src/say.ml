
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let good_start =
  F.x "I'm ready" []

let bad_start =
  F.x "Something went wrong" []

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

let action f =
  begin function
  | `None -> ()
  | `Rock_Take -> ()
  | `Rock_Drop -> ()
  | `Wall_In -> f wall_in
  | `Rock_In -> f rock_in
  | `Exit_In -> f exit_in
  | `Web_In -> f web_in
  | `Web_Out -> f web_out
  | `Exit -> f exit
  | `No_Exit -> f no_exit
  | `Carry_Exit -> f carry_exit
  | `Rock_No_Take -> f rock_no_take
  | `Rock_No_Drop -> f rock_no_drop
  end

