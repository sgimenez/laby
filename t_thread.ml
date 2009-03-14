(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

(**
   Native thread management module.
*)

let mutex () =
  let m = Mutex.create () in
  object
    method lock = Mutex.lock m;
    method unlock = Mutex.unlock m;
  end

let exec l =
  let threads = List.map (fun f -> Thread.create f ()) l in
  List.iter Thread.join threads

let main =
  T.mutex_ref := mutex;
  T.exec_ref := exec
