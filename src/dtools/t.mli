(*
   Copyright (C) 2007-2010 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

(**
   Thread management module.
*)

type mutex = < lock: unit; unlock: unit; >

val mutex : unit -> mutex
val exec : (unit -> unit) list -> unit

(** / *)
val mutex_ref : (unit -> mutex) ref
val exec_ref : ((unit -> unit) list -> unit) ref
