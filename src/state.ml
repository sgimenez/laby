type terrain = [ `Void | `Wall | `Exit | `Rock | `Web | `NRock | `NWeb ]
type dir = [ `N | `E | `S | `W ]

type action =
 [ `None
 | `Start
 | `Wall_In
 | `Rock_In
 | `Exit_In
 | `Web_In
 | `Web_Out
 | `Exit
 | `No_Exit
 | `Carry_Exit
 | `Rock_Take
 | `Rock_Drop
 | `Rock_No_Take
 | `Rock_No_Drop
 ]

type t =
    {
      map: terrain array array;
      pos: int * int;
      dir: dir;
      carry: [`None | `Rock ];
      action: action;
    }

let copy state =
  let map =
    Array.init (Array.length state.map) (fun j -> Array.copy state.map.(j))
  in
  { state with map = map }

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
  { state with action = `None; }

let chg state action =
  { state with action = action }

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

let forward ?(toexit=false) state =
  let move pos =
    let pos' = front state in
    begin match get state pos, get state pos' with
    | `Web, _ -> pos, `Web_Out
    | _, `Rock -> pos, `Rock_In
    | _, `Wall -> pos, `Wall_In
    | _, `Exit -> if toexit then (pos', `Exit) else (pos, `Exit_In)
    | _, `Web -> pos', `Web_In
    | _, _ -> pos', `None
    end
  in
  let pos, action = move state.pos in
  { state with pos = pos; action = action }

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
      | `None, `Exit -> "ok", forward ~toexit:true state
      | _, `Exit -> "error", chg state `Carry_Exit
      | _, _ -> "error", chg state `No_Exit
      end
  | "take" ->
      begin match state.carry, get state (front state) with
      | `None, `Rock ->
	  "ok",
	  { state with
            map = set state (front state) `Void;
            carry = `Rock;
	    action = `Rock_Take;
	  }
      |  _, _ -> "error", chg state `Rock_No_Take
      end
  | "drop" ->
      begin match state.carry, get state (front state) with
      | `Rock, `Void ->
	  "ok",
	  { state with
            map = set state (front state) `Rock;
            carry = `None;
	    action = `Rock_Drop;
	  }
      |  _, _ -> "error", chg state `Rock_No_Drop
      end
  | a ->
      log_protocol#error (
	F.x "unknown action: <action>" [
	  "action", F.string a;
	];
      );
     "-", state
  end

let size state =
  Array.length state.map.(0),
  Array.length state.map
