
(*
 * Copyright (C) 2007-2014 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

val play :
  msg: (string -> unit) ->
  help: (string option -> unit) ->
  draw: (State.t -> unit) ->
  <
    chg_mod: Mod.t -> string;
    chg_level: Level.t -> unit;
    chg_program: string -> unit;
    run : bool;
    next : bool;
    prev : bool;
    quit : unit;
  >
