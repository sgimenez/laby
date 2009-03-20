
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

let conf_tags =
  Conf.void (F.x "theme" [])

let conf =
  Conf.void (F.x "display configuration" [])

let conf_texts =
  Conf.string ~p:(conf#plug "texts") ~d:"texts"
    (F.x "path for texts" [])

let conf_theme =
  Conf.string ~p:(conf#plug "theme") ~d:"theme.conf"
    (F.x "path for theme" [])

let env_term =
  begin try Some (Sys.getenv "TERM") with Not_found -> None end

let lang =
  begin try Sys.getenv "LC_MESSAGES" with Not_found ->
    begin try Sys.getenv "LC_ALL" with Not_found ->
      begin try Sys.getenv "LANG" with Not_found ->
	""
      end
    end
  end

let escape c = "\027[" ^ c ^ "m", "\027[0m"

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
    if !tstr = None then
      tstr := Some (String.concat ";" (List.map codes c#get));
    begin match !tstr with
    | Some tstr when tstr <> "" ->
	let esc1, esc2 = escape tstr in
	F.h ~sep:F.n [F.s esc1; m ; F.s esc2]
    | _ -> m
    end
  in
  F.t tag

let load_theme log path =
  Conf.load log ~strict:false conf_tags#ut (path ^ conf_theme#get)

let tag_quoted = tag "format-quoted"
let tag_label = tag "format-label"

let tag_msg_unknown = tag "format-msg-unknown"
let tag_msg_lang = tag "format-msg-lang"
let tag_msg_bad = tag "format-msg-bad"

let tag_msg_lang lang =
  F.t (fun x -> F.h [tag_msg_lang (F.s ("<" ^ lang ^ ">")); x])

let texts : (string * string, (string * F.t) list -> F.t) Hashtbl.t =
  Hashtbl.create 1024

let texts_line key str =
  let msg_lang = ref "" in
  let special = ref "" in
  let txt = ref "" in
  let elem = ref "" in
  let arg = ref "" in
  let l = ref [] in
  let push c str = str := !str ^ String.make 1 c in
  let add_elem () =
    if !elem <> "" then  l := !l @ [ `Elem !elem ]; elem := ""
  in
  let add_arg () = l := !l @ [ `Arg !arg ]; arg := "" in
  let p = ref 0 in
  let eos = String.length str in
  let state = ref `Init in
  while !p < eos do
    state :=
      begin match !state, str.[!p] with
      | `Init, '\t' ->
	  while str.[!p] = '\t' do incr p done;
	  if !msg_lang = "text"
	  then `Text
	  else `Elem
      | `Init, ':' ->
	  if !msg_lang <> "text"
	  then `Error
	  else (
	    incr p;
	    let i = !p in
	    while str.[!p] <> '\t' do incr p done;
	    special := String.sub str i (!p - i);
	    while str.[!p] = '\t' do incr p done;
	    `Text
	  )
      | `Init, ' ' -> `Error
      | `Init, c -> push c msg_lang; incr p; `Init
      | `Text, '\n' -> incr p; `Text
      | `Text, c -> incr p; push c txt; `Text
      | `Elem, '<' -> incr p; add_elem (); `Arg
      | `Elem, '\n' -> incr p; add_elem (); `Elem
      | `Elem, c -> push c elem; incr p; `Elem
      | `Arg, '>' -> incr p; add_arg (); `Elem
      | `Arg, '\n' -> `Error
      | `Arg, c -> push c arg; incr p; `Arg
      | `Error, _ -> p := eos; `Error
      end
  done;
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
    let msg = F.b (List.map map !l) in
    if !msg_lang = lang then msg
    else tag_msg_lang !msg_lang  msg
  in
  begin match !state with
  | `Text -> key := (!special, !txt); `Skip
  | `Elem -> `Entry (!msg_lang, !key, msg)
  | _ ->
      if (str <> "\n" && str.[0] <> '#') then `Error else `Skip
  end

let bad key vars =
  let arg (var, fn) = F.h [tag_msg_bad (F.s (var ^ ":")); fn] in
  let fn vars =
    F.h [tag_msg_bad (F.s "<!>"); F.s (snd key); F.v (List.map arg vars)]
  in
  Hashtbl.add texts key fn;
  fn vars


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
	    begin try (Hashtbl.find texts key) vars with
	    | Not_found ->
		begin match
		  texts_line (ref key) ("\t" ^ mstr ^ "\n")
		with
		| `Entry (_, _, msg) ->
	            Hashtbl.add texts key msg; msg vars
		| _ -> bad key vars
		end
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
	    (date.Unix.tm_year+1900)
	    (date.Unix.tm_mon+1)
	    date.Unix.tm_mday
	    date.Unix.tm_hour
	    date.Unix.tm_min
	    date.Unix.tm_sec
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

let stdout x =
  Printf.printf "%s\n" (render_color x)

let read_texts log path file =
  let error line f =
    log#error (
      F.x "file <f>, line <l>: <error>" [
	"f", F.string path;
	"l", F.int line;
	"error", f
      ]
    )
  in
  let lnb = ref 0 in
  begin try
    let key = ref ("", "") in
    while true do
      let str = incr lnb; input_line file ^ "\n" in
      begin match texts_line key str with
      | `Entry (msg_lang, key, msg) ->
	  log#debug 5 (
	    F.x "message special=<id> lang=<lang>: <text>" [
	      "id", F.string (fst key);
	      "lang", F.string msg_lang;
	      "text", F.q (F.s (snd key));
	    ]
	  );
	  if lang = msg_lang then
	    Hashtbl.add texts key msg
      | `Skip -> ()
      | `Error -> error !lnb (F.x "syntax error" [])
      end
    done
  with
  | End_of_file -> ()
  end

let load_texts log path =
  let path = path ^ conf_texts#get in
  begin try
    let file = open_in_bin path in
    read_texts log path file;
    close_in file;
  with
  | Sys_error m ->
      log#warning (
	F.x "failed to open theme file: <error>"
	  ["error", F.string m]
      );
  end

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
  stdout (F.h [F.h ~sep:F.n [F.x "exception" []; F.s ":"]; F.q (F.exn e)])

let init log path =
  load_theme log path;
  load_texts log path
