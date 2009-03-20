(*
   Copyright (C) 2007-2009 Stéphane Gimenez
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

val lang : string
  (**
     Locale string.
  *)

val theme: Init.t
  (**
     Atom to load theme.
  *)

val texts: Init.t
  (**
     Atom to load texts.
  *)
