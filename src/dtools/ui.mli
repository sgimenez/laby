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
