exception Invalid
exception Conflict
exception Error of F.t

type action =
    | Noop
    | Excl of (unit -> unit)
    | Do of (unit -> unit)

type handle = unit -> action
type handle_s = string -> action
type t = char * string * handle option * handle_s option * F.t

let noshort = '\000'
let nolong = ""

let color = F.t "opt.option"

let l_f long = color (F.s ("--" ^ long))
let s_f short = color (F.s ("-" ^ String.make 1 short))

let f (short, long, _, _, _) =
  begin match short = noshort, long = nolong with
  | true, true -> assert false
  | true, false -> l_f long
  | false, true -> s_f short
  | false, false -> F.h [l_f long; F.s "("; s_f short; F.s ")"]
  end

let help_opt opts : t =
  let handle () =
    let action () =
      let msg = F.x "display help message" [] in
      let help_opt : t = ('h', "help", None, None, msg) in
      let usage = help_opt :: opts in
      let pl opt =
	let (short, long, _, _, desc) = opt in
	F.f [f opt; F.q desc]
      in
      let program = F.s (Sys.argv.(0)) in
      let options_f = F.v (List.map pl usage) in
      F.print ~l:"usage" (
	F.h [program; F.s " "; F.s "[options]"; F.s " "; options_f]
      );
    in
    Excl action
  in
  'h', "help", Some handle, None, F.n

let parse () =
  let i = ref 0 in
  let args = ref [] in
  let opts = ref [] in
  let getarg () =
    incr i;
    if !i >= Array.length Sys.argv
    then None
    else Some (Sys.argv.(!i))
  in
  let add_arg arg = args := arg :: !args in
  let add_opt opt = opts := opt :: !opts in
  let rec aux state =
    begin match state, getarg () with
    | `None, None -> ()
    | `None, Some arg ->
	let l = String.length arg in
	begin match l with
	| 0 | 1 -> add_arg arg; aux `None
	| _ ->
	    begin match arg.[0], arg.[1] with
	    | '-', '-' ->
		if l = 2 then aux (`AllArgs)
		else
		  begin try
		      let i = String.index arg '=' in
		      let opt = String.sub arg 2 (i-2) in
		      let value = String.sub arg (i+1) (l-i-1) in
		      add_opt (`Long (opt, Some value))
		    with
		    | Not_found ->
			let opt = String.sub arg 2 (l-2) in
			add_opt (`Long (opt, None))
		  end;
		aux `None
	    | '-', opt ->
		if l = 2
		then (add_opt (`Short (opt, None)); aux `None)
		else
		  begin match arg.[2] with
		  | '=' ->
		      let value = String.sub arg (3) (l-3) in
		      add_opt (`Short (opt, Some value));
		      aux `None
		  | _ ->
		      for i = 1 to l - 1 do
			add_opt (`Short (arg.[i], None))
		      done;
		      aux `None
		  end
	    | _, _ -> add_arg arg; aux `None
	    end
	end
    | `AllArgs, None -> ()
    | `AllArgs, Some arg -> add_arg arg; aux `AllArgs
    end
  in
  aux `None;
  List.rev !args, List.rev !opts

let cmd opts main =
  let actions = ref [] in
  let excl_command = ref None in
  let set_excl_command c =
    begin match !excl_command with
    | None -> excl_command := Some c
    | Some _ ->
	raise (Error (F.x "multiple exclusive actions specified" []));
    end
  in
  let action (opt : t) value =
    let (short, long, handle, handle_s, descr) = opt in
    let add_action =
      begin function
      | Noop -> ()
      | Excl f -> set_excl_command f
      | Do f -> actions := !actions @ [f]
      end
    in
    let catch fn a s =
      begin try fn a with
      | Invalid ->
	  raise (Error (
	    F.x "invalid argument to <opt>: <arg>" [
	      "opt", f opt;
	      "arg", F.sq s;
	    ]
	  ))
      | Conflict ->
	  raise (Error (
	    F.x "option <opt> conflicts with a previous option" [
	      "opt", f opt;
	    ]
	  ))
      end
    in
    begin match value with
    | None ->
	begin match handle with
	| None ->
	    raise (Error (
	      F.x "option <opt> requires an argument" [
		"opt", f opt;
	      ]
	    ))
	| Some a ->
	    fun () -> add_action (catch a () "")
	end
    | Some s ->
	begin match handle_s with
	| None ->
	    raise (Error (
	      F.x "option <opt> does not take an argument" [
		"opt", f opt;
	      ]
	    ))
	| Some h ->
	    fun () -> add_action (catch h s s)
	end
    end
  in
  let opts = help_opt opts :: opts in
  let argl, optl = parse () in
  let unk_opt f =
    raise (Error (
      F.x "unknown option <opt>" [
	"opt", f;
      ]
    ))
  in
  let find_short c =
    begin match List.filter (fun (c0,_,_,_,_) -> c0 = c) opts with
    | [] -> unk_opt (s_f c)
    | [opt] -> opt
    | _ -> assert false
    end
  in
  let find_long s =
    begin match List.filter (fun (_,s0,_,_,_) -> s0 = s) opts with
    | [] -> unk_opt (l_f s)
    | [opt] -> opt
    | _ -> assert false
    end
  in
  let do_opt =
    begin function
    | `Short (c, v) -> action (find_short c) v
    | `Long (s, v) -> action (find_long s) v
    end
  in
  let actions = List.map do_opt optl in
  List.iter (fun f -> f ()) actions;
  begin match !excl_command with
  | None -> main argl
  | Some f -> f ()
  end

let debug_opt =
  let debug i =
    F.set_debug (Some i);
    Run.catch := Some (F.exn);
  in
  let handle () = debug 1; Noop in
  let handle_s slevel =
    begin try debug (int_of_string slevel); Noop with
    | Failure _ -> raise Invalid
    end
  in
  'd', "debug", Some handle, Some handle_s,
  F.x "output debug information" []

let log_opt ~default =
  let r = ref None in
  let action () =
    begin match !r with
    | None -> ()
    | Some path -> F.log path
    end
  in
  let handle () =
    if !r <> None then raise Conflict;
    r := default; Do (action)
  in
  let handle_s path =
    begin try
	if !r <> None then raise Conflict;
      r := Some path; Do (action)
      with
      | Failure _ -> raise Invalid
    end
  in
  'l', "log", Some handle, Some handle_s,
  F.x "log output to a file" []

let theme_opt ~default =
  let r = ref None in
  let action () =
    begin match !r with
    | None -> ()
    | Some path -> F.set_theme path
    end
  in
  let handle () =
    if !r <> None then raise Conflict;
    r := default; Do (action)
  in
  let handle_s path =
    begin try
	if !r <> None then raise Conflict;
      r := Some path; Do (action)
      with
      | Failure _ -> raise Invalid
    end
  in
  noshort, "theme", Some handle, Some handle_s,
  F.x "select theme file" []
