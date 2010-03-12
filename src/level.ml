
(*
 * Copyright (C) 2007-2010 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

type t =
    {
      map: State.tile array array;
      pos: int * int;
      dir: State.dir;
      mrocks: (int * int) list;
      mwebs: (int * int) list;
      title: string;
      comment: string;
      help: string;
    }

let log = Log.make ["level"]

let dummy =
  {
    map =
      [|
	[| `Wall; `Wall; `Wall; `Wall |];
	[| `Wall; `Void; `Void; `Exit |];
	[| `Wall; `Void; `Void; `Wall |];
	[| `Wall; `Void; `Void; `Wall |];
	[| `Wall; `Wall; `Wall; `Wall |];
      |];
    pos = 1, 3; dir = `N;
    mrocks = []; mwebs = [];
    title = ""; comment = ""; help = ""
  }

let sep = Str.regexp " "

let get_map lines =
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
	Run.fatal (
	  F.x "unknown tile" [];
	);
    end
  in
  let tr line =
    posx := -1; incr posy;
    Array.of_list (List.map conv (Str.split sep line))
  in
  let map = Array.of_list (List.map tr lines) in
  map, !antpos, !dir, !may_rocks, !may_webs

let rec get_sentence default lines =
  begin match lines with
  | [] -> default
  | l :: q ->
      begin try
	let lang_str = String.sub l 0 (String.index l '\t') in
	let last = String.rindex l '\t' in
	let comment_str =
	  String.sub l (last + 1) (String.length l - last - 1)
	in
	if default = "" || lang_str = Ui.conf_lang#get
	then get_sentence comment_str q
	else get_sentence default q
      with
      | Not_found -> get_sentence default q
      end
  end

let load file =
  let sections = Res.read_blocks file in
  let map, antpos, dir, mrocks, mwebs =
    get_map (match sections "map:" with None -> [] | Some l -> l)
  in
  let title =
    begin match sections "title:" with
    | None -> "?"
    | Some l -> get_sentence "" l
    end
  in
  let comment =
    begin match sections "comment:" with
    | None -> "?"
    | Some l -> get_sentence "" l
    end
  in
  let help =
    begin match sections "help:" with
    | None -> ""
    | Some l -> String.concat " " l
    end
  in
  {
    map = map; pos = antpos; dir = dir;
    mrocks = mrocks; mwebs = mwebs;
    title = title; comment = comment; help = help;
  }

let comment level =
  level.comment

let title level =
  level.title

let help level =
  level.help

let generate level =
  let map =
    Array.init (Array.length level.map) (fun j -> Array.copy level.map.(j))
  in
  let fill m tile =
    begin match m with
    | [] -> ()
    | _ ->
	let i = Random.int (List.length m) in
	let x, y = List.nth m i in
	map.(y).(x) <- tile
    end
  in
  fill level.mrocks `Rock;
  fill level.mwebs `NWeb;
  State.make map level.pos level.dir

let size level =
  Array.fold_left (fun m e -> max m (Array.length e)) 0 level.map,
  Array.length level.map
