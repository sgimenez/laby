
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

val conf : Conf.ut
val conf_tilesize : int Conf.t

exception Error of F.t

val display_gtk : ?language_list:string list -> unit -> unit
