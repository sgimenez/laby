type terrain = [ `Void | `Wall | `Exit | `Rock | `Pit | `NRock | `NPit ]
type dir = [ `N | `E | `S | `W ]

type t =
    {
      map: terrain array array;
      pos: int * int;
      dir: dir;
      carry: [`None | `Rock ];
      say: string;
    }

let level = ref 0

let basic =
  {
    map =
      [|
	[| `Wall; `Wall; `Wall; `Wall; `Wall |];
	[| `Wall; `Void; `Void; `Void; `Exit |];
	[| `Wall; `Void; `Void; `Void; `Wall |];
	[| `Wall; `Void; `Void; `Void; `Wall |];
	[| `Wall; `Void; `Void; `Rock; `Wall |];
	[| `Wall; `Wall; `Wall; `Wall; `Wall |];
      |];
    pos = 1, 4;
    dir = `N;
    carry = `None;
    say = "";
  }

let copy state =
  { state with
    map =
      Array.init (Array.length state.map) (fun j -> Array.copy state.map.(j));
    say = String.copy(state.say);
  }

let get state (i, j) =
  if i >= 0 && j >= 0
    && j < Array.length state.map && i < Array.length (state.map.(0))
  then state.map.(j).(i)
  else `Wall

let set state (i, j) t =
  if i >= 0 && j >= 0
    && j < Array.length state.map && i < Array.length (state.map.(0))
  then
    let map = Array.copy state.map in
    let row = Array.copy map.(j) in
    row.(i) <- t;
    map.(j) <- row;
    map
  else state.map

let say state s =
  { state with say = s }

let left state =
  let turn =
    begin function `N -> `W | `W -> `S | `S -> `E | `E -> `N end
  in
  { state with dir = turn state.dir }

let right state =
  let turn =
    begin function `N -> `E | `E -> `S | `S -> `W | `W -> `N end
  in
  { state with dir = turn state.dir }

let front state =
  begin match state.dir with
  | `N -> fst state.pos, snd state.pos - 1
  | `E -> fst state.pos + 1, snd state.pos
  | `S -> fst state.pos, snd state.pos + 1
  | `W -> fst state.pos - 1, snd state.pos
  end

let forward state =
  let move pos =
    let pos' = front state in
    begin match get state pos' with
    | `Void | `Pit | `NPit | `NRock ->
	if get state pos <> `Pit then pos' else pos
    | _ -> pos
    end
  in
  { state with pos = move state.pos }

let look state =
  get state (front state)

let load file =
  let f = open_in file in
  let l = input_line f in
  let sizex, sizey, posx, posy, dir =
    Scanf.sscanf l "%d %d %d %d %s" (fun i1 i2 i3 i4 s1 ->
      i1, i2, i3, i4,
      begin match s1 with
      | "N" -> `N
      | "E" -> `E
      | "S" -> `S
      | "W" -> `W
      | _ -> Printf.eprintf "unknown direction\n"; `N
      end
    )
  in
  let map = Array.make_matrix sizey sizex `Void in
  let rand () =
    let b = ref (Random.bool ()) in
    fun a1 a2 -> b := not !b; if !b then a1 else a2
  in
  let rock = rand () in
  let pit = rand () in
  begin
    for j = 0 to sizey - 1 do
      let s = input_line f in
      for i = 0 to sizex - 1 do
	map.(j).(i) <-
	  begin match s.[i] with
	  | 'w' -> `Wall;
	  | 'e' -> `Exit;
	  | 'r' -> `Rock;
	  | 'R' -> rock `Rock `NRock;
	  | 'p' -> `Pit;
	  | 'P' -> pit `Pit `NPit;
	  | _ -> `Void;
	  end
      done;
      done;
  end;
  close_in f;
  {
    map = map;
    pos = posx, posy;
    dir = dir;
    carry = `None;
    say = "";
  }

let output channels s =
  Printf.fprintf (Unix.out_channel_of_descr (snd channels)) "%s\n%!" s

let run (input, output) =
  let rec next state =
    begin match input () with
    | None -> None
    | Some "forward" -> Some (forward state)
    | Some "left" -> Some (left state)
    | Some "right" -> Some (right state)
    | Some "look" ->
	let ans =
	  begin match look state with
	  | `NRock | `NPit | `Void -> "void"
	  | `Wall -> "wall"
	  | `Rock -> "rock"
	  | `Pit -> "pit"
	  | `Exit -> "exit"
	  end
	in
	output ans; next state
    | Some "open" ->
	begin match state.carry, get state (front state) with
	| `None, `Exit ->
	    begin
	      incr level;
	      let file =
		Config.conf_path ^ Printf.sprintf "levels/level%d.map" !level
	      in
	      if Sys.file_exists file then Some (load file)
	      else (Printf.eprintf "no more levels\n%!"; None)
	    end
	| _, `Exit -> Some (say state "!")
	| _, _ -> Some (say state "?")
	end
    | Some "take" ->
	begin match state.carry, get state (front state) with
	| `None, `Rock ->
	    Some
	      { state with
		map = set state (front state) `Void;
		carry = `Rock;
	      }
	|  _, _ -> Some (say state "!")
	end
    | Some "drop" ->
	begin match state.carry, get state (front state) with
	| `Rock, `Void ->
	    Some
	      { state with
		map = set state (front state) `Rock;
		carry = `None;
	      }
	|  _, _ -> Some (say state "!")
	end
    | Some a -> Printf.eprintf "unknown action: %s\n%!" a; None
    end
  in next
