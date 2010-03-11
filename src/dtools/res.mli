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
   Resource management module.
*)

val conf : Conf.ut
  (**
     Resource configuration key.
  *)

val conf_paths : string list Conf.t
  (**
     Global resource paths.
  *)

exception Error of F.t
  (**
     Execption raised when resources cannot be found/acceced.
  *)

type t = string list
  (**
     Resources paths.
  *)

val get : t -> string
  (**
     Return a filename for the given resource.
  *)

val get_list : ?ext:string -> t -> string list
  (**
     Return a list of subresources (with given extention) for the
     given resource.
  *)

val use : t -> (string -> in_channel -> unit) -> unit
  (**
     Easy resource accessor.
  *)
