let input_ch, output_ch =
  stdin, stdout

let output s =
  Printf.fprintf output_ch "%s\n%!" s

let input () =
  begin try
      input_line input_ch
    with
    | End_of_file -> exit 1
  end

let gauche () = output "left"

let droite () = output "right"

let avance () = output "forward"

let regarde () = output "look";
  begin match input () with
  | "void" -> `Vide
  | "wall" -> `Mur
  | "rock" -> `Roche
  | "web" -> `Toile
  | "exit" -> `Sortie
  | s -> Printf.eprintf "robot: unknown tile : %s\n" s; assert false
  end

let ouvre () = output "open"

let prend () = output "take"

let pose () = output "drop"

let pousse () = output "push"
