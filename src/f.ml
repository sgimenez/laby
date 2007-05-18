type d =
    | N
    | S of string
    | F of string * t list
    | H of string * t list
    | V of string * t list
    | P of int * (string * string) option * t
    | T of string * t
and t = unit -> d

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

let escape c s = "\027[" ^ c ^ "m" ^ s ^ "\027[0m"

let color, bcolor =
  begin match env_term with
  | Some ("xterm" | "xterm-color") ->
      let c r g b = string_of_int (16+b+6*g+36*r) in
      (fun r g b -> "38;5;" ^ c r g b), (fun r g b -> "48;5;" ^ c r g b)
  | Some ("rxvt-unicode" | "rxvt") ->
      let c r g b = string_of_int (16+(b*3/5)+4*(g*3/5)+16*(r*3/5)) in
      (fun r g b -> "38;5;" ^ c r g b), (fun r g b -> "48;5;" ^ c r g b)
  | _ ->
      (fun r g b -> "0"), (fun r g b -> "0")
  end

let tags : (string, string) Hashtbl.t =
  Hashtbl.create 1024
let texts : (string, (string * t) list -> t) Hashtbl.t =
  Hashtbl.create 1024

let punct c =
  c = ',' || c = ':' || c = '!' || c = '?' || c = ';' || c = '.'
  || c = '(' || c = ')'

let force l = l ()

let rec str ?(ind="\n") ?(pri=100) t =
  let aux ?(ind=ind) ?(pri=pri) tl = str ~ind ~pri tl in
  begin match t with
  | N -> ""
  | S (s) -> s
  | F (ind0, l) ->
      let ind = ind ^ ind0 in
      let rec f s r =
	begin function
	| [] -> s
	| N :: q -> f s r q
	| S s' :: q ->
	    if s' = "" then f s r q else
	      let sep =
		if r <> "" then r ^ " " else
		  if s' = "" || punct s'.[0] || s = "" then "" else " "
	      in
	      f (s ^ sep ^ s') "" q
	| x :: q ->
	    let s' = aux ~ind x in
	    if s' = "" then f s r q else
	      let sep, r' =
		begin match x with
		| V _ -> "", ind
		| _ -> (if s = "" then "" else " "), ""
		end
	      in
	      f (r ^ s ^ sep ^ aux ~ind x) r' q
	end
      in
      f "" "" (List.map force l)
  | H (sep0, l) ->
      let sl = List.map (fun x -> aux x) (List.map force l) in
      String.concat sep0 sl
  | V (head0, l) ->
      let ind' = ind ^ head0 in
      let rec f s =
	begin function
	| [] -> s
	| N :: q -> f s q
	| ((V _) as x) :: q -> f (s ^ aux ~ind:ind' x) q
	| x :: q -> f (s ^ ind' ^ aux ~ind:ind' x) q
	end
      in
      f "" (List.map force l)
  | P (pri0, delim, t) ->
      let s = aux ~pri:pri0 (force t) in
      begin match delim with
      | None -> s
      | Some (d_begin, d_end) ->
	  if pri <= pri0 then d_begin ^ s ^ d_end else s
      end
  | T (tag, t) ->
      let s = aux (force t) in
      begin try
	  let tstr = Hashtbl.find tags tag in
	  if tstr <> "" then escape tstr s else s
	with
	| Not_found -> s
      end
  end

let string t = str (t ())

let n =
  fun () -> N
let s string =
  fun () -> S string

let t tag t =
  fun () -> T (tag, t)
let f ?(ind=(s "")) l =
  fun () -> F (string ind, l)
let h ?(sep=n) tl =
  fun () -> H (string sep, tl)
let v ?(head=(s "  ")) tl =
  fun () -> V (string head, tl)
let p i ?(delim=(s "("),(s ")")) t =
  let b, e = delim in
  fun () -> P (i, Some (string b, string e), t)
let pn i t =
  fun () -> P (i, None, t)

let q l =
  v [l]


let sb ?(padding=0) string =
  h [s (Printf.sprintf "%-0*s" padding string)]
let sq string =
  h [s "\""; t "format.quoted" (s string); s "\""]

let i ?zeros int =
  begin match zeros with
  | None -> s (string_of_int int)
  | Some x -> s (Printf.sprintf "%0*i" x int)
  end

let float ?zeros v =
  begin match zeros with
  | None -> s (string_of_float v)
  | Some x -> s (Printf.sprintf "%0*f" x v)
  end

let l string b = f [t "format.label" (s (string ^ ":")); b]


let texts_line txt str =
  let msg_lang = ref "" in
  let elem = ref "" in
  let arg = ref "" in
  let l = ref [] in
  let push c str = str := !str ^ String.make 1 c in
  let add_elem () = l := !l @ [ `Elem !elem ]; elem := "" in
  let add_arg () = l := !l @ [ `Arg !arg ]; arg := "" in
  let p = ref 0 in
  let eos = String.length str in
  let state = ref `Init in
  while !p < eos do
    state :=
      begin match !state, str.[!p] with
      | `Init, '\t' ->
	  while str.[!p] = '\t' do incr p done;
	  if !msg_lang = "text" then (txt := ""; `Text) else `Elem
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
      | `Elem e -> s e
      | `Arg a ->
	  begin try List.assoc a vars with
	  | Not_found -> t "msg.unknown" (s "<?>")
	  end
      end
    in
    if !msg_lang = lang then h (List.map map !l)
    else h (t "msg.lang" (s ("<" ^ !msg_lang ^ ">")) :: List.map map !l)
  in
  begin match !state with
  | `Text -> `Skip
  | `Elem -> `Entry (!msg_lang, !txt, msg)
  | _ ->
      if (str <> "\n" && str.[0] <> '#') then `Error else `Skip
  end

let bad m vars =
  let arg (var, fn) = f [t "msg.bad" (s (var ^ ":")); fn] in
  let fn vars =
    f [t "msg.bad" (s "<!>"); s m; v (List.map arg vars)]
  in
  Hashtbl.add texts m fn;
  fn vars

let x m vars () : d =
  begin try
      begin try (Hashtbl.find texts m) vars () with
      | Not_found ->
	  begin match texts_line (ref m) ("\t" ^ m ^ "\n") with
	  | `Entry (msg_lang, txt, msg) ->
	      Hashtbl.add texts txt msg; msg vars ()
	  | _ -> bad m vars ()
	  end
      end
    with
    | Not_found -> bad m vars ()
  end

let debug = ref None
let debug_level = ref 0
let set_debug d =
  debug := d;
  begin match d with
  | None -> debug_level := 0
  | Some i -> debug_level := i
  end

let output = ref (print_string)

let log path =
  let open_opts =
    [Unix.O_WRONLY; Unix.O_NONBLOCK; Unix.O_CREAT; Unix.O_APPEND]
  in
  let fd = Unix.openfile path open_opts 0o644 in
  let f = Unix.out_channel_of_descr fd in
  let o s =
    let time = Printf.sprintf "%f" (Unix.gettimeofday ()) in
    output_string f (time ^ " " ^ s)
  in
  output := o

let print ?l ?d ?e =
  let head =
    let l1 =
      begin match l with
      | None -> []
      | Some lbl -> [t "format.label" (s (lbl ^ ":"))]
      end
    in
    let l2 =
      begin match d with
      | None -> []
      | Some lvl -> [t "format.debug" (s (string_of_int lvl ^ ":"))]
      end
    in
    let l3 =
      begin match e with
      | None ->  []
      | Some 0 -> [t "format.fatal-error" (s "error:")]
      | Some 1 -> [t "format.error" (s "error:")]
      | Some _ -> [t "format.warning" (s "warning:")]
      end
    in
    string (f (l1 @ l2 @ l3))
  in
  let head = if head = "" then "" else head ^ " " in
  begin match d with
  | None ->
      (fun tu -> !output ((head ^ string tu) ^ "\n"))
  | Some level when level <= !debug_level ->
      (fun tu -> !output ((head ^ string tu) ^ "\n"))
  | _ -> (fun _ -> ())
  end

let prompt tu =
  print_string (string tu);
  let s =
    begin try Some (read_line ()) with
    |  End_of_file -> None
    end
  in
  print_string "\r";
  s

let exn exn =
  let head = [t "format.internal-error" (s "internal error:")] in
  !output ((string (f head)) ^ " " ^ Printexc.to_string exn ^ "\n")

let codes error =
  let is_digit c = '0' <= c && c <= '5' in
  let digit c = int_of_char c - int_of_char '0' in
  let unrecognized str =
    error (x "unrecognized value <val>" ["val", sq str]);
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

let theme_line error string =
  let key = ref "" in
  let elem = ref "" in
  let code = ref "" in
  let push c s = s := !s ^ String.make 1 c in
  let add_code s =
    code := !code ^ (if !code = "" || s = "" then "" else ";") ^ s
  in
  let spacer c = c = ' ' || c = '\t' in
  let p = ref 0 in
  let eos = String.length string in
  let state = ref `Init in
  while !p < eos do
    state :=
      begin match !state, string.[!p] with
      | `Init, c when spacer c -> incr p; `Init
      | `Init, c -> `Key
      | `Key, c when spacer c -> `Post_Key
      | `Key, ':' -> `Post_Key
      | `Key, '\n' -> `Error
      | `Key, c -> push c key; incr p; `Key
      | `Post_Key, c when spacer c -> incr p; `Post_Key
      | `Post_Key, ':' -> incr p; `Elem
      | `Post_Key, _ -> `Error
      | `Elem, (':' | '\n') ->
	  add_code (codes error !elem); elem := ""; incr p; `Elem
      | `Elem, c -> incr p; push c elem; `Elem
      | `Error, _ -> p := eos; `Error
      end
  done;
  begin match !state with
  | `Error ->
      if (string <> "\n" && string.[0] <> '#')
      then error (x "syntax error" [])
  | _ ->
      print ~d:5 (
	f [s "key:"; sq !key; s "code"; sq !code]
      );
      Hashtbl.add tags !key !code
  end

let read_theme error f =
  let lnb = ref 0 in
  begin try
      while true do
	let s = incr lnb; input_line f ^ "\n" in
	theme_line (error !lnb) s
      done
    with
    | End_of_file -> ()
  end

let read_texts error file =
  let lnb = ref 0 in
  begin try
      let txt = ref "" in
      while true do
	let string = incr lnb; input_line file ^ "\n" in
	begin match texts_line txt string with
	| `Entry (msg_lang, txt, msg) ->
	    print ~d:5 (
	      f [s "msg txt:"; sq txt; s "lang:"; sq msg_lang]
	    );
	    if lang = msg_lang then
	      Hashtbl.add texts txt msg
	| `Skip -> ()
	| `Error -> error !lnb (x "syntax error" [])
	end
      done
    with
    | End_of_file -> ()
  end

let error path line f =
  print ~e:2 (
    x "file <f>, line <l>: <error>" [
      "f", sq path;
      "l", i line;
      "error", f
    ];
  )

let set_theme path =
  let print = print ~l:"theme" in
  begin try
      let f = open_in_bin path in
      read_theme (error path) f;
      close_in f
    with
    | Sys_error m ->
	print ~e:2 (
	  x "open failed: <error>" ["error", sq m]
	);
	print ~e:2 (
	  x "failed to open theme file" []
	);
  end

let set_texts path =
  let print = print ~l:"texts" in
  begin try
      let f = open_in_bin path in
      read_texts (error path) f;
      close_in f;
    with
    | Sys_error m ->
	print ~e:2 (x "open failed: <error>" ["error", sq m]);
	print ~e:2 (x "failed to open texts file" []);
  end
