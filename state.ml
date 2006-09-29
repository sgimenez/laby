type terrain = [ `Void | `Wall | `Exit ]
type dir = [ `N | `E | `S | `W ]


type t =
    {
      map: terrain array array;
      pos: int * int * dir;
    }

let basic =
  {
    map =
      [|
	[| `Wall; `Wall; `Wall; `Wall; `Wall |];
	[| `Wall; `Void; `Void; `Void; `Wall |];
	[| `Wall; `Void; `Void; `Void; `Wall |];
	[| `Wall; `Void; `Void; `Void; `Wall |];
	[| `Wall; `Void; `Void; `Void; `Exit |];
	[| `Wall; `Wall; `Wall; `Wall; `Wall |];
      |];
    pos = 1, 1, `S;
  }

(*
let load file =
  let f = open_in file in
  let s = input_line f in
  begin try
      while true do
      done;
    with
    | End_of_file -> ()
  end
*)
