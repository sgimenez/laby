let print = F.print ~l:"opt"

exception Invalid
exception Conflict
exception Error

type action =
    | Noop
    | Excl of (unit -> unit)
    | Do of (unit -> unit)

type handle = unit -> action
type handle_s = string -> action
type t = char * string * handle option * handle_s option * string

let noshort = Getopt.noshort
let nolong = Getopt.nolong

let f (short, long, _, _, _) =
  let color = F.t "getopt.option" in
  let l_f = color (F.s ("--" ^ long)) in
  let s_f = color (F.s ("-" ^ String.make 1 short)) in
  begin match short = noshort, long = nolong with
  | true, true -> assert false
  | true, false -> l_f
  | false, true -> s_f
  | false, false -> F.h [l_f; F.s "("; s_f; F.s ")"]
  end

let help_opt opts =
  let handle () =
    let action () =
      let usage =
	('h', "help", None, None, "displays help message") :: opts
      in
      let pl opt =
	let (short, long, _, _, desc) = opt in
	F.f [f opt; F.v [F.s desc]]
      in
      let program = F.s (Sys.argv.(0)) in
      let options_f = F.v (List.map pl usage) in
      F.print ~l:"usage" (fun () ->
	F.h [program; F.s " "; F.s "[options]"; F.s " "; options_f]
      );
    in
    Excl action
  in
  'h', "help", Some handle, None, ""

let cmd opts arg main =
  let actions = ref [] in
  let excl_command = ref None in
  let set_excl_command c =
    begin match !excl_command with
    | None -> excl_command := Some c
    | Some _ ->
	print ~e:1 (fun () ->
	  F.text "multiple exclusive actions specified" []
	);
	raise Error
    end
  in
  let opt_to_getopt opt =
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
	  print ~e:1 (fun () ->
	    F.text "invalid argument to <opt>: <arg>" [
		"opt",  f opt;
		"arg", F.sq s;
	    ]
	  );
	  raise Error
      | Conflict ->
	  print ~e:1 (fun () ->
	    F.text "option <opt> conflicts with a previous option" [
		"opt", f opt;
	    ]
	  );
	  raise Error
      end
    in
    let handle' =
      begin match handle with
      | None -> None
      | Some a -> Some (fun () -> add_action (catch a () ""))
      end
    in
    let handle_s' =
      begin match handle_s with
      | None -> None
      | Some h -> Some (fun s -> add_action (catch h s s))
      end
    in
    (short, long, handle', handle_s')
  in
  let getopts = List.map opt_to_getopt ((help_opt opts) :: opts) in
  begin try
      Getopt.parse_cmdline getopts arg;
    with
    | Getopt.Error m ->
	print ~e:1 (fun () -> F.s m); raise Error
  end;
  List.iter (fun f -> f ()) !actions;
  begin match !excl_command with
  | None -> main ()
  | Some f -> f ()
  end

let debug_opt =
  let debug i =
    F.set_debug (Some i);
    Run.catch := Some (F.exn);
  in
  let handle () = debug 1; Noop
  in
  let handle_s slevel =
    begin try debug (int_of_string slevel); Noop with
    | Failure _ -> raise Invalid
    end
  in
  'd', "debug", Some handle, Some handle_s,
  "outputs debug information"

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
  "logs output to a file"

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
  "chooses theme file"
