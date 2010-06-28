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
   Ui management module.
*)

val conf : Conf.ut
  (**
     Ui configuration key.
  *)

val conf_lang : string Conf.t
  (**
     Locale string.
  *)

val theme: Srv.t
  (**
     Atom to load theme.
  *)

val texts: Srv.t
  (**
     Atom to load texts.
  *)

val read_text : string -> string list -> string
