
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

(**
   Formated display management module.
*)

val conf : Conf.ut
val init : F.logger -> string -> unit

val lang : string

val tag : string -> F.t -> F.t

val render_raw : F.t -> string
val render_color : F.t -> string

val stdout : F.t -> unit

val output : F.t -> unit
val input : unit -> string option

val exn : exn -> unit

