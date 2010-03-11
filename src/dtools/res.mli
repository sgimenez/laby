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

val conf_domain : string Conf.t
  (**
     Global resource domain.
  *)

val conf_data_dirs : string list Conf.t
  (**
     Global resource paths.
  *)

val conf_bin_dirs : string list Conf.t
  (**
     Global resource paths.
  *)

exception Error of F.t
  (**
     Execption raised when resources cannot be found/accessed.
  *)


type path = string

val path : string list -> path


(* Data units *)


type t = string list
  (**
     Resource unit
  *)

val get : t -> path
  (**
     Return a filename for the given resource unit.
  *)

val get_list : ?ext:string -> t -> string list
  (**
     Return a list of subresources (with given extention) for the
     given resource unit.
  *)


(* Easy resource accessors *)


val read_chan : path -> (in_channel -> unit) -> unit

val read_full : path -> string

val read_lines : path -> string list

val read_blocks : path -> string -> string list option


(* Binaries *)

val get_bin : string -> path option

