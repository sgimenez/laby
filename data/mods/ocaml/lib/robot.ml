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

let laby_name_left () =
  output "left"; ignore (input ())

let laby_name_right () =
  output "right"; ignore (input ())

let laby_name_forward () =
  output "forward"; ignore (input ())

let laby_name_look () : [ `laby_name_Void | `laby_name_Wall | `laby_name_Rock | `laby_name_Web | `laby_name_Exit ]
    =
  output "look";
  begin match input () with
  | "void" -> `laby_name_Void
  | "wall" -> `laby_name_Wall
  | "rock" -> `laby_name_Rock
  | "web" -> `laby_name_Web
  | "exit" -> `laby_name_Exit
  | s -> Printf.eprintf "robot: unknown tile : %s\n" s; assert false
  end

let laby_name_door_open () =
  output "open"; ignore (input ())

let laby_name_take () =
  output "take"; ignore (input ())

let laby_name_drop () =
  output "drop"; ignore (input ())

let _ =
  output "start"; ignore (input ())
