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
    | _, `Exit -> pos, Some "exit-in"
    | _, `Web -> pos', Some "web-in"
    | _, _ -> pos', None
    end
  in
  let pos, sound = move state.pos in
  { state with pos = pos; sound = sound }

let forward_open state =
  let move pos =
    let pos' = front state in
    begin match get state pos, get state pos' with
    | `Web, _ -> pos, Some "web-out"
    | _, `Exit -> pos', Some "exit"
    | _, _ -> pos, None
    end
  in
  let pos, sound = move state.pos in
  { state with pos = pos; sound = sound }

let look state =
  get state (front state)

let log_protocol = Log.make ["protocol"]

let output channels s =
  Printf.fprintf (Unix.out_channel_of_descr (snd channels)) "%s\n%!" s

let run action state =
  let state = clean state in
  begin match action with
  | "forward" -> "ok", forward state
  | "left" -> "ok", left state
  | "right" -> "ok", right state
  | "look" ->
      let ans =
	begin match look state with
	| `NRock | `NWeb | `Void -> "void"
	| `Wall -> "wall"
	| `Rock -> "rock"
	| `Web -> "web"
	| `Exit -> "exit"
	end
      in
      ans, state
  | "open" ->
      begin match state.carry, get state (front state) with
      | `None, `Exit -> "ok", forward_open state
      | _, `Exit -> "error", chg state "!" "bad"
      | _, _ -> "error", chg state "?" "bad"
      end
  | "take" ->
      begin match state.carry, get state (front state) with
      | `None, `Rock ->
	  "ok",
	  { state with
            map = set state (front state) `Void;
            carry = `Rock;
            sound = Some "rock-take";
	  }
      |  _, _ -> "error", chg state "!" "bad"
      end
  | "drop" ->
      begin match state.carry, get state (front state) with
      | `Rock, `Void ->
	  "ok",
	  { state with
            map = set state (front state) `Rock;
            carry = `None;
            sound = Some "rock-drop";
	  }
      |  _, _ -> "error", chg state "!" "bad"
      end
  | a ->
      log_protocol#error (
	F.x "unknown action: <action>" [
	  "action", F.string a;
	];
      );
     "-", state
  end
