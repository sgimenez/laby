#!/usr/bin/env ocaml
#load "unix.cma"
#load "robot.cmo"

let _ =
  (* Niveau 0 *)
  Robot.droite ();
  Robot.avance ();
  Robot.prend ();
  Robot.gauche ();
  Robot.avance ();
  Robot.pose ();
  Robot.droite ();
  Robot.avance ();
  Robot.gauche ();
  Robot.avance ();
  Robot.avance ();
  Robot.droite ();
  Robot.ouvre ();

  (* Niveau 1 *)
  Robot.droite ();
  while Robot.regarde () = `Vide do Robot.avance () done;
  Robot.droite ();
  Robot.ouvre ();

  (* Niveau 2 *)
  let fonce () =
    while Robot.regarde () = `Vide do Robot.avance () done
  in
  Robot.droite ();
  fonce ();
  Robot.gauche ();
  fonce ();
  Robot.gauche ();
  fonce ();
  Robot.droite ();
  fonce ();
  Robot.droite ();
  fonce ();
  Robot.ouvre ()
