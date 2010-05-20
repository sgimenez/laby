
(*
 * Copyright (C) 2007-2010 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

val conf : Conf.ut
val conf_selected : string Conf.t
val conf_exclusive : bool Conf.t

val opt : Opt.t

type query = string * (string -> unit)

type t =
    <
      name: string;
      check: bool;
      set_buf: string -> unit;
      get_buf: string;
      start: (string -> unit) -> bool;
      probe: (string -> unit) -> query option;
      stop: unit;
      help: string -> string;
    >

val pool : unit -> t list

val dummy : string -> t
