#!/usr/bin/env ocaml
#load "unix.cma"
#load "robot.cmo"

let _ =
  Robot.avance ();
  Robot.avance ();
  Robot.avance ();
  Robot.droite ();
  Robot.avance ();
  Robot.avance ();
  Robot.avance ();

  Robot.debut "level1";
  Robot.droite ();
  while Robot.regarde () = `Rien do Robot.avance () done;
  Robot.avance ();

  Robot.debut "level2";
  let fonce () =
    while Robot.regarde () = `Rien do Robot.avance () done
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
  fonce ()
