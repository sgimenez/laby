let input_ch, output_ch =
  Unix.open_process "./fdls"

let output s =
  Printf.fprintf output_ch "%s\n%!" s

let input () =
  input_line input_ch

let gauche () = output "left"

let droite () = output "right"

let avance () = output "forward"

let regarde () = output "look";
  begin match input () with
  | "void" -> `Rien
  | "wall" -> `Mur
  | "exit" -> `Sortie
  | _ -> assert false
  end

let debut s = output "load"; output s
