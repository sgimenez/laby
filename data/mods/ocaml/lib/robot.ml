type tile =
  | laby_name_Void | laby_name_Wall | laby_name_Rock
  | laby_name_Web | laby_name_Exit | laby_name_Unknown

let act s f =
  Printf.fprintf stdout "%s\n%!" s;
  begin try
    begin match input_line stdin with
    | "quit" -> exit 0
    | x -> f x
    end
  with
  | End_of_file -> exit 1
  end

let laby_name_left () =
  act "left" ignore

let laby_name_right () =
  act "right" ignore

let laby_name_forward () =
  act "forward" ignore

let laby_name_take () =
  act "take" ignore

let laby_name_drop () =
  act "drop" ignore

let laby_name_escape () =
  act "escape" ignore

let laby_name_look () =
  act "look"
    begin function
    | "void" -> laby_name_Void
    | "wall" -> laby_name_Wall
    | "rock" -> laby_name_Rock
    | "web" -> laby_name_Web
    | "exit" -> laby_name_Exit
    | _ -> laby_name_Unknown
    end

let laby_name_say s =
  act ("say " ^ s) ignore

let _ =
  act "start" ignore
