
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
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
