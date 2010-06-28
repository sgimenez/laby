(*
   Copyright (C) 2007-2010 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

let conf_tags =
  Conf.void (F.x "theme" [])

let texts : (string * string, string * ((string * F.t) list -> F.t)) Hashtbl.t =
  Hashtbl.create 1024

let env_term =
  begin try Some (Sys.getenv "TERM") with Not_found -> None end

let escape =
  begin match env_term with
  | Some ("xterm" | "xterm-color"
    | "rxvt-256color" | "rxvt-unicode" | "rxvt") ->
      (fun c -> "\027[" ^ c ^ "m", "\027[0m")
  | _ -> (fun _ -> "", "")
  end

let color, bcolor =
  begin match env_term with
  | Some ("xterm" | "xterm-color" | "rxvt-256color") ->
      let c r g b = string_of_int (16+b+6*g+36*r) in
      (fun r g b -> "38;5;" ^ c r g b), (fun r g b -> "48;5;" ^ c r g b)
  | Some ("rxvt-unicode" | "rxvt") ->
      let c r g b = string_of_int (16+(b*3/5)+4*(g*3/5)+16*(r*3/5)) in
      (fun r g b -> "38;5;" ^ c r g b), (fun r g b -> "48;5;" ^ c r g b)
  | _ ->
      (fun r g b -> "0"), (fun r g b -> "0")
  end

let codes =
  let is_digit c = '0' <= c && c <= '5' in
  let digit c = int_of_char c - int_of_char '0' in
  let unrecognized str =
    (* error (F.x "unrecognized value <val>" ["val", F.string str]); *)
    ""
  in
  begin function
  | "" -> ""
  | "bold" -> "01" | "italic" -> "03" | "underline" -> "04"
  | "#black" -> "30" | "##black" -> "40"
  | "#red" -> "31" | "##red" -> "41"
  | "#green" -> "32" | "##green" -> "42"
  | "#yellow" -> "33" | "##yellow" -> "43"
  | "#blue" -> "34" | "##blue" -> "44"
  | "#purple" -> "35" | "##purple" -> "45"
  | "#cyan" -> "36" | "##cyan" -> "46"
  | "#white" -> "37" | "##white" -> "47"
  | str when str.[0] = '#' ->
      if String.length str = 4 &&
	is_digit str.[1] && is_digit str.[2] && is_digit str.[3]
      then color (digit str.[1]) (digit str.[2]) (digit str.[3])
      else if String.length str = 5 && str.[1] = '#'
	&& is_digit str.[2] && is_digit str.[3] && is_digit str.[4]
      then bcolor (digit str.[2]) (digit str.[3]) (digit str.[4])
      else unrecognized str
  | str -> unrecognized str
  end

let tag s =
  let c =
    begin try Conf.list ~p:(conf_tags#plug s) ~d:[] F.n
    with
    | Conf.Unbound _ ->
	Printf.printf "name: %s%!\n" s; assert false
    end
  in
  let tstr = ref None in
  let tag m =
    (* if !tstr = None then *)
      tstr := Some (String.concat ";" (List.map codes c#get));
    begin match !tstr with
    | Some tstr when tstr <> "" ->
	let esc1, esc2 = escape tstr in
	F.b [F.s esc1; m ; F.s esc2]
    | _ -> m
    end
  in
  F.t tag

let tag_quoted = tag "format-quoted"
let tag_label = tag "format-label"

let tag_msg_unknown = tag "format-msg-unknown"
let tag_msg_bad = tag "format-msg-bad"

let bad key =
  let arg (var, fn) = F.h [tag_msg_bad (F.s (var ^ ":")); fn] in
  fun vars ->
    F.h [tag_msg_bad (F.s "<!>"); F.s (snd key); F.v (List.map arg vars)]

let list_of_string s =
  let rec aux x s l =
    if x < 0 then l else aux (x - 1) s (s.[x] :: l)
  in
  aux (String.length s - 1) s []

let get_text_locale key =
  try fst (Hashtbl.find texts key) with Not_found -> ""

let def_text key (locale, str) =
  let b = Buffer.create 256 in
  let push c = Buffer.add_char b c in
  let take () = let c = Buffer.contents b in Buffer.clear b; c in
  let rec elem stream out =
    begin match stream with
    | '>' :: q -> `Error
    | '<' :: q ->
	begin match take () with
	| "" -> arg q out
	| s -> arg q (`Elem s :: out)
	end
    | '\n' :: q -> `Error
    |  c :: q -> push c; elem q out
    | _ ->
	let l =
	  begin match take () with
	  | "" -> out
	  | s -> `Elem s :: out
	  end
	in `Ok (List.rev l)
    end
  and arg stream out =
    begin match stream with
    | '>' :: q ->
	begin match take () with
	| "" -> elem q out
	| s -> elem q (`Arg s :: out)
	end
    | '<' :: q -> `Error
    | '\n' :: q -> `Error
    |  c :: q -> push c; arg q out
    | _ -> `Error
    end
  in
  begin match elem (list_of_string str) [] with
  | `Ok list ->
      let msg vars =
	let map =
	  begin function
	  | `Elem e -> F.s e
	  | `Arg a ->
	      begin try List.assoc a vars with
	      | Not_found -> tag_msg_unknown (F.s "<?>")
	      end
	  end
	in
	F.b (List.map map list)
      in
      Hashtbl.add texts key (locale, msg)
  | `Error ->
      Hashtbl.add texts key (locale, bad key)
  end

let string format t =
  let add state string =
    let ind, sep, pri, s = state in
    (ind, "", pri, s ^ sep ^ string)
  in
  let withind ind (_, sep, pri, s) = (ind, sep, pri, s) in
  let withindsep ind sep (_, _, pri, s) = (ind, sep, pri, s) in
  let withfbsep ind0 sep' (ind, sep, pri, s) =
    ind, (if sep = "" then sep' else ind0), pri, s
  in
  let withpri pri (ind, sep, _, s) = (ind, sep, pri, s) in
  let rec str t state =
    let box m =
      let _, _, _, b = str m ("\n", "", 10000, "") in
      b
    in
    begin match F.use t with
    | `N -> state
    | `T (t, m) -> if format = `Color then str (t m) state else str m state
    | `S (s') -> add state s'
    | `L (slabel, m) ->
	let ind, sep, pri, s = state in
	let b = box (tag_label (F.s (slabel ^ ":"))) in
	str m (ind, " ", pri, s ^ sep ^ b)
    | `H (sep0, []) -> state
    | `H (sep0, l) ->
	let ind, _, _, _ = state in
	let sep0 = box sep0 in
	let cat state m =
	  withfbsep ind sep0 (str m state)
	in
	let state0 = withind (ind ^ "  ") state in
	let state' = List.fold_left cat state0 l in
	withindsep ind "" state'
    | `V (head0, []) -> state
    | `V (head0, l) ->
	let ind, _, _, _ = state in
	let head0 = box head0 in
	let ind' = ind ^ head0 in
	let cat state m = str m (withindsep ind' ind' state) in
	let state0 = withindsep ind ind state in
	let state' = List.fold_left cat state0 l in
	withindsep ind ind state'
    | `Q l ->
	str (F.v [l]) state
    | `P (pri0, wrap, m) ->
	let _, _, pri, s = state in
	let m' = if pri <= pri0 then wrap m else m in
	withpri pri (str m' (withpri pri0 state))
    | `X (special, mstr, vars) ->
	let key = (special, mstr) in
	let t =
	  begin try
	    begin try (snd (Hashtbl.find texts key)) vars with
	    | Not_found ->
		def_text key ("", mstr);
		(snd (Hashtbl.find texts key)) vars
	    end
	  with
	  | Not_found -> bad key vars
	  end
	in
	str t state
    | `Int i -> add state (string_of_int i)
    | `Float f -> add state (string_of_float f)
    | `String s ->
	let sstring = box (tag_quoted (F.s s)) in
	add state ("\"" ^ sstring ^ "\"")
    | `Time time ->
	let date = Unix.localtime time in
	let stime =
	  Printf.sprintf "%d/%02d/%02d %02d:%02d:%02d"
	    (date.Unix.tm_year + 1900)
	    (date.Unix.tm_mon + 1) date.Unix.tm_mday
	    date.Unix.tm_hour date.Unix.tm_min date.Unix.tm_sec
	in
	add state stime
    | `Exn e ->
	add state (Printexc.to_string e)
    | `Lazy fn ->
	str (fn ()) state
    | _ -> add state "<unknown>"
    end
  in
  let _, _, _, s = str t ("\n", "", 10000, "") in
  s

let render_raw x = string `Raw x
let render_color x = string `Color x

let render fd x =
  if Unix.isatty fd then string `Color x else string `Raw x

let stdout x =
  Printf.printf "%s\n" (render Unix.stdout x)

let stderr x =
  Printf.eprintf "%s\n" (render Unix.stderr x)

let tag_cmd_output = tag "cmd-output"
let tag_cmd_input = tag "cmd-input"

let output f =
  let head = F.b [tag_cmd_output (F.s ">"); F.s " "] in
  stdout (F.v ~head [f]);
  stdout F.n

let input () =
  let prompt = F.h [tag_cmd_input (F.s "<"); F.s ""] in
  print_string (render_color prompt);
  begin try Some (read_line ()) with
  | End_of_file -> print_newline (); None
  end

let exn e =
  stdout (F.h [F.b [F.x "exception" []; F.s ":"]; F.q (F.exn e)])

