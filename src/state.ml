
(*
 * Copyright (C) 2007-2010 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

type tile = [ `Void | `Wall | `Exit | `Rock | `Web | `NRock | `NWeb ]
type dir = [ `N | `E | `S | `W ]

type action =
 [ `None
 | `Wall_In | `Rock_In | `Exit_In | `Web_In
 | `Web_Out
 | `Exit | `No_Exit | `Carry_Exit
 | `Rock_Take | `Rock_Drop
 | `Take_Nothing | `Drop_Nothing | `Drop_No_Space
 | `Say of string
 ]

type t =
    {
      map: tile array array;
      pos: int * int;
      dir: dir;
      carry: [`None | `Rock ];
      action: action;
    }

let make map pos dir =
  {
    map = map; pos = pos; dir = dir;
    carry = `None; action = `None;
  }

let iter_map s p =
    Array.iteri (fun j a -> Array.iteri (fun i t -> p i j t) a) s.map

let pos s = s.pos
let dir s = s.dir
let carry s = s.carry
let action s = s.action

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
  | "escape" ->
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
      |  _, _ -> "error", chg state `Take_Nothing
      end
  | "drop" ->
      begin match state.carry, get state (front state) with
      | `Rock, `Void
      | `Rock, `Web ->
	  "ok",
	  { state with
            map = set state (front state) `Rock;
            carry = `None;
	    action = `Rock_Drop;
	  }
      | `None, _ -> "error", chg state `Drop_Nothing
      |  _, _ -> "error", chg state `Drop_No_Space
      end
  | a when String.length a > 4 && String.sub a 0 4 = "say " ->
      "ok", chg state (`Say (String.sub a 4 (String.length a - 4)))
  | a ->
      log_protocol#error (
	F.x "unknown action: <action>" [
	  "action", F.string a;
	];
      );
     "-", state
  end

let size state =
  Array.fold_left (fun m e -> max m (Array.length e)) 0 state.map,
  Array.length state.map
