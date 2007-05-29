type terrain = [ `Void | `Wall | `Exit | `Rock | `Web | `NRock | `NWeb ]
type dir = [ `N | `E | `S | `W ]

type t =
    {
      map: terrain array array;
      pos: int * int;
      dir: dir;
      carry: [`None | `Rock ];
      say: string;
      sound: string option;
    }

let level = ref 0

let basic =
  {
    map =
      [|
	[| `Wall; `Wall; `Wall; `Wall; `Wall |];
	[| `Wall;  `Web; `Void; `Void; `Exit |];
	[| `Wall; `Void; `Void; `Void; `Wall |];
	[| `Wall; `Void; `Void; `Void; `Wall |];
	[| `Wall; `Void; `Void; `Rock; `Wall |];
	[| `Wall; `Wall; `Wall; `Wall; `Wall |];
      |];
    pos = 1, 4;
    dir = `N;
    carry = `None;
    say = "";
    sound = None;
  }

let copy state =
  { state with
    map =
      Array.init (Array.length state.map) (fun j -> Array.copy state.map.(j));
    say = String.copy state.say;
    sound =
      begin match state.sound with
      | None -> None
      | Some s -> Some (String.copy s)
      end
    ;
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

let clean state =
  { state with say = ""; sound = None; }

let chg state say sound =
  { state with say = say; sound = Some sound; }

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
    begin match get state pos, get state pos' with
    | `Web, _ -> pos, Some "web-out"
    | _, `Rock -> pos, Some "rock-in"
    | _, `Wall -> pos, Some "wall-in"
    | _, `Exit -> pos', Some "exit"
    | _, `Web -> pos', Some "web-in"
    | _, _ -> pos', None
    end
  in
  let pos, sound = move state.pos in
  { state with pos = pos; sound = sound }

let look state =
  get state (front state)

let sep = Str.regexp " "

let load file =
  let f = open_in file in
  let lines = ref [] in
  let posx = ref (-1) in
  let posy = ref (-1) in
  let may_rocks = ref [] in
  let may_webs = ref [] in
  let dir = ref `N in
  let antpos = ref (0, 0) in
  let conv s =
    incr posx;
    begin match s with
    | "o" -> `Wall;
    | "x" -> `Exit;
    | "↑" -> antpos := !posx, !posy; dir := `N; `Void;
    | "→" -> antpos := !posx, !posy; dir := `E; `Void;
    | "↓" -> antpos := !posx, !posy; dir := `S; `Void;
    | "←" -> antpos := !posx, !posy; dir := `W; `Void;
    | "r" -> `Rock;
    | "R" -> may_rocks := (!posx, !posy) :: !may_rocks; `NRock;
    | "w" -> `Web;
    | "W" -> may_webs := (!posx, !posy) :: !may_webs; `Web;
    | "." -> `Void;
    | _ ->
        F.print ~e:1 (
	  F.x "level: unknown tile" [];
	);
	Run.exit ();
    end
  in
  begin try
      while true do
	posx := -1; incr posy;
	let l = Array.of_list (Str.split sep (input_line f)) in
	if l <> [||] then lines := (Array.map conv l) :: !lines;
      done;
    with
    | End_of_file -> ()
  end;
  let sizex = Array.length (List.hd !lines) in
  begin match List.for_all (fun a -> Array.length a = sizex) !lines with
  | false ->
      F.print ~e:1 (
	F.x "level: mismatching line length" [];
      );
      Run.exit ();
  | true -> ()
  end;
  let array = Array.of_list (List.rev !lines) in
  let fill may tile =
  if !may <> [] then
    begin
      let i = Random.int (List.length !may) in
      let x, y = List.nth !may i in
      array.(y).(x) <- tile;
    end;
  in
  fill may_rocks `Rock;
  fill may_webs `NWeb;
  close_in f;
  {
    map = array;
    pos = !antpos;
    dir = !dir;
    carry = `None;
    say = "";
    sound = None;
  }

let output channels s =
  Printf.fprintf (Unix.out_channel_of_descr (snd channels)) "%s\n%!" s

let run (input, output) =
  let rec next state =
    let state = clean state in
    begin match input () with
    | None -> None
    | Some "forward" -> Some (forward state)
    | Some "left" -> Some (left state)
    | Some "right" -> Some (right state)
    | Some "look" ->
	let ans =
	  begin match look state with
	  | `NRock | `NWeb | `Void -> "void"
	  | `Wall -> "wall"
	  | `Rock -> "rock"
	  | `Web -> "web"
	  | `Exit -> "exit"
	  end
	in
	output ans; next state
    | Some "open" ->
	begin match state.carry, get state (front state) with
	| `None, `Exit -> Some (forward state)
	| _, `Exit -> Some (chg state "!" "bad")
	| _, _ -> Some (chg state "?" "bad")
	end
    | Some "take" ->
	begin match state.carry, get state (front state) with
	| `None, `Rock ->
	    Some
	      { state with
		map = set state (front state) `Void;
		carry = `Rock;
		sound = Some "rock-take";
	      }
	|  _, _ -> Some (chg state "!" "bad")
	end
    | Some "drop" ->
	begin match state.carry, get state (front state) with
	| `Rock, `Void ->
	    Some
	      { state with
		map = set state (front state) `Rock;
		carry = `None;
		sound = Some "rock-drop";
	      }
	|  _, _ -> Some (chg state "!" "bad")
	end
    | Some a ->
	F.print ~e:2 (
	  F.x "unknown action: <action>" [
	      "action", F.sq a;
	  ];
	);
	None
    end
  in
  next
