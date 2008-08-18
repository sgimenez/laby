type t =
    {
      map: State.terrain array array;
      pos: int * int;
      dir: State.dir;
      mrocks: (int * int) list;
      mwebs: (int * int) list;
      comment: string;
    }

let log = Log.make ["level"]

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
    mrocks = [];
    mwebs = [];
    comment = ""
  }

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
  let comment = ref "" in
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
        log#fatal (
	  F.x "unknown tile" [];
	);
	Run.exit ();
    end
  in
  let rec get_lines () =
    posx := -1; incr posy;
    let l = Array.of_list (Str.split sep (input_line f)) in
    if l <> [||]
    then (lines := (Array.map conv l) :: !lines; get_lines ())
  in
  let rec get_comment () =
    posx := -1; incr posy;
    let l = input_line f in
    let re = Str.regexp (Printf.sprintf "\\([^ ]*\\) \\(.*\\)") in
    if Str.string_match re l 0
    then (
      if !comment = "" || Str.matched_group 1 l = Fd.lang
      then comment := Str.matched_group 2 l
      else get_comment ()
    )
    else get_comment ()
  in
  begin try
    get_lines ();
    get_comment ();
  with
  | End_of_file -> ()
  end;
  let sizex = Array.length (List.hd !lines) in
  begin match List.for_all (fun a -> Array.length a = sizex) !lines with
  | false ->
      log#fatal (
	F.x "level: mismatching line length" [];
      );
      Run.exit ();
  | true -> ()
  end;
  let array = Array.of_list (List.rev !lines) in
  close_in f;
  {
    map = array;
    pos = !antpos;
    dir = !dir;
    mrocks = !may_rocks;
    mwebs = !may_webs;
    comment = !comment;
  }

let comment level =
  level.comment

let generate level =
  let map =
    Array.init (Array.length level.map) (fun j -> Array.copy level.map.(j))
  in
  let fill m tile =
  if m <> [] then
    begin
      let i = Random.int (List.length m) in
      let x, y = List.nth m i in
      map.(y).(x) <- tile;
    end;
  in
  fill level.mrocks `Rock;
  fill level.mwebs `NWeb;
  {
    State.map = map;
    State.pos = level.pos;
    State.dir = level.dir;
    State.carry = `None;
    State.action = `None;
  }

let size state =
  Array.length state.map.(0),
  Array.length state.map
