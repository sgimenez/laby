(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

type mutex = < lock: unit; unlock: unit; >

let dummy_mutex () =
  object
    method lock = ();
    method unlock = ();
  end

let mutex_ref = ref dummy_mutex

let mutex () = !mutex_ref ()

let dummy_exec l =
  List.iter (fun f -> f ()) l

let exec_ref = ref dummy_exec

let exec l = !exec_ref l

