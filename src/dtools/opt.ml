(*
   Copyright (C) 2007-2009 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

let tag_option = Fd.tag "opt-option"

type action =
  | Undefined
  | Invalid of F.t
  | Error of F.t
  | Excl of (unit -> F.t)
  | Do of (unit -> unit)

type spec =
  [
  | `None
  | `Any of string option -> action
  | `Arg of string -> action
  | `Do_unit of unit -> unit
  | `Excl_unit of unit -> F.t
  | `Do_string of string -> unit
  | `Excl_string of string -> F.t
  | `Do_int of int -> unit
  | `Excl_int of int -> F.t
  | `Do_optint of int option -> unit
  | `Excl_optint of int option -> F.t
  ]

type t = char option * string option * spec * F.t

let make ?short ?long spec descr =
  assert (short <> Some '-');
  assert (long <> Some "");
  short, long, spec, descr

let l_f long = tag_option (F.s ("--" ^ long))
let s_f short = tag_option (F.s ("-" ^ String.make 1 short))

let f ?arg (short, long, _, _) =
  let optf =
    begin match short, long with
    | None, None -> assert false
    | None, Some l -> l_f l
    | Some s, None -> s_f s
    | Some s, Some l ->
	F.h ~sep:F.n [l_f l; F.s "("; s_f s; F.s ")"]
    end
  in
  begin match arg with
  | None -> optf
  | Some arg -> F.b [optf; F.s "="; F.string arg]
  end

let help_opt opts : t =
  let action () =
    let msg = F.x "display help message" [] in
    let help_opt : t = (Some 'h', Some "help", `None, msg) in
    let usage = help_opt :: opts in
    let pl opt =
      let (short, long, _, desc) = opt in
      F.h [f opt; F.q desc]
    in
    let program = F.s (Sys.argv.(0)) in
    let options_f = List.map pl usage in
    F.l "usage" (
      F.h [program; F.s "[options]"; F.v options_f]
    )
  in
  Some 'h', Some "help", `Excl_unit action, F.n

let parse argv =
  let i = ref 0 in
  let args = ref [] in
  let opts = ref [] in
  let getarg () =
    incr i;
    if !i >= Array.length argv
    then None
    else Some (argv.(!i))
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

let wrong_value kind =
  Invalid (F.x "<type> value expected" ["type", F.s kind])

let wrap_spec sp =
  begin match sp with
  | `None -> assert false
  | `Any a -> a
  | `Arg f ->
      begin function
      | None -> Undefined
      | Some s -> f s
      end
  | `Do_unit f ->
      begin function
      | None -> Do f
      | Some _ -> Undefined
      end
  | `Excl_unit f ->
      begin function
      | None -> Excl f
      | Some _ -> Undefined
      end
  | `Do_string f ->
      begin function
      | None -> Undefined
      | Some s -> Do (fun () -> f s)
      end
  | `Excl_string f ->
      begin function
      | None -> Undefined
      | Some s -> Excl (fun () -> f s)
      end
  | `Do_int f ->
      begin function
      | None -> Undefined
      | Some s ->
	  begin try
	    let i = int_of_string s in
	    Do (fun () -> f i)
	  with
	  | Failure "int_of_string" -> wrong_value "int"
	  end
      end
  | `Excl_int f ->
      begin function
      | None -> Undefined
      | Some s ->
	  begin try
	    let i = int_of_string s in
	    Excl (fun () -> f i)
	  with
	  | Failure "int_of_string" -> wrong_value "int"
	  end
      end
  | `Do_optint f ->
      begin function
      | None -> Do (fun () -> f None)
      | Some s ->
	  begin try
	    let i = int_of_string s in
	    Do (fun () -> f (Some i))
	  with
	  | Failure "int_of_string" -> wrong_value "int"
	  end
      end
  | `Excl_optint f ->
      begin function
      | None -> Excl (fun () -> f None)
      | Some s ->
	  begin try
	    let i = int_of_string s in
	    Excl (fun () -> f (Some i))
	  with
	  | Failure "int_of_string" -> wrong_value "int"
	  end
      end
  end

let cmd ?(argv=Sys.argv) opts =
  let actions = ref [] in
  let errors = ref [] in
  let commands = ref [] in
  let set_err m = errors := m :: !errors in
  let set_command c = commands := c :: !commands in
  let action (opt : t) value =
    let (short, long, spec, descr) = opt in
    begin match wrap_spec spec value with
      | Undefined when value = None ->
	  set_err (
	    F.x "option <opt> requires an argument" [
	      "opt", f opt;
	    ]
	  )
      | Undefined ->
	  set_err (
	    F.x "option <opt> does not take an argument" [
	      "opt", f opt;
	    ]
	  )
      | Invalid m ->
	  set_err (
	    F.x "invalid argument to <opt>: <error>" [
	      "opt", f ?arg:value opt;
	      "error", F.q m;
	    ]
	  )
      | Error m ->
	  set_err (
	    F.x "error with <opt>: <error>" [
	      "opt", f ?arg:value opt;
	      "error", F.q m;
	    ]
	  )
      | Excl fn -> set_command fn
      | Do fn -> actions := !actions @ [fn]
    end
  in
  let opts = help_opt opts :: opts in
  let argl, optl = parse argv in
  let unk_opt opt_f =
    set_err (
      F.x "unknown option <opt>" [
	"opt", opt_f;
      ]
    )
  in
  let find_short c action v =
    begin match List.filter (fun (c0, _, _, _) -> c0 = Some c) opts with
    | [] -> unk_opt (s_f c)
    | [opt] -> action opt v
    | _ -> assert false
    end
  in
  let find_long s action v =
    begin match List.filter (fun (_, s0, _, _) -> s0 = Some s) opts with
    | [] -> unk_opt (l_f s)
    | [opt] -> action opt v
    | _ -> assert false
    end
  in
  let do_opt =
    begin function
    | `Short (c, v) -> find_short c action v
    | `Long (s, v) -> find_long s action v
    end
  in
  List.iter do_opt optl;
  List.iter (fun fn -> fn ()) (List.rev (!actions));
  begin match !errors, !commands with
  | [], [] -> `Proceed argl
  | [], [e] -> `Excl e
  | errors, excl ->
      let errors =
	if List.length excl > 1 then
	  (F.x "multiple exclusive actions specified" []) :: errors
	else
	  errors
      in
      `Errors (List.rev errors)
  end

let opt_unit ?short ?long conf : t =
  make ?short ?long (`Do_unit conf#set) conf#descr

let opt_int ?short ?long conf : t =
  let action s =
    begin try
      let i = int_of_string s in
      Do (fun () -> conf#set i)
    with
    | Failure "int_of_string" -> wrong_value "int"
    end
  in
  make ?short ?long (`Arg action) conf#descr

let opt_float ?short ?long conf : t =
  let action s =
    begin try
      let f = float_of_string s in
      Do (fun () -> conf#set f)
    with
    | Failure "float_of_string" -> wrong_value "float"
    end
  in
  make ?short ?long (`Arg action) conf#descr

let opt_bool ?short ?long conf : t =
  let set =
    begin function
    | None | Some "true" -> Do (fun () -> conf#set true)
    | Some "false" -> Do (fun () -> conf#set false)
    | Some _ -> wrong_value "bool"
    end
  in
  make ?short ?long (`Any set) conf#descr

let opt_string ?short ?long conf : t =
  make ?short ?long (`Do_string conf#set) conf#descr

let opt_list ?short ?long conf : t =
  let action s = conf#set (conf#get @ [s]) in
  make ?short ?long (`Do_string action) conf#descr

let conf ?short ?long conf : t =
  begin match conf#kind with
  | Some "unit" -> opt_unit ?short ?long (Conf.as_unit conf#ut)
  | Some "int" -> opt_int ?short ?long (Conf.as_int conf#ut)
  | Some "bool" -> opt_bool ?short ?long (Conf.as_bool conf#ut)
  | Some "float" -> opt_float ?short ?long (Conf.as_float conf#ut)
  | Some "string" -> opt_string ?short ?long (Conf.as_string conf#ut)
  | Some "list" -> opt_list ?short ?long (Conf.as_list conf#ut)
  | _ -> assert false
  end

let unknown_key_error s =
  Error (F.x "unknown key <key>" ["key", F.string s])
let mismatch_error k =
  begin match k#kind with
  | None ->
      Error (F.x "this key cannot be assigned a value" []);
  | Some s ->
      Error (F.x "expected type for this key is <type>" ["type", F.string s])
  end

let conf_set ?short ?long conf : t =
  let arg p =
    begin try Do (Conf.set conf p) with
      | Conf.Unbound (_ , s) -> unknown_key_error s
      | Conf.Wrong_Conf (_, f) -> Error f
    end
  in
  make ?short ?long (`Arg arg)
    (F.x "set a configuration key" [])

let conf_descr ?short ?long t =
  let set =
    begin function
    | None ->
	Excl (fun () -> F.v (Conf.descr t))
    | Some p ->
	begin try
	  let dl = Conf.descr ~prefix:(Conf.path_of_string p) t in
	  Excl (fun () -> F.v dl)
	with
	| Conf.Unbound (_, s) -> unknown_key_error s
	end
    end
  in
  make ?short ?long (`Any set)
    (F.x "describe a configuration key" [])

let conf_dump ?short ?long t =
  let set =
    begin function
    | None -> Excl (fun () -> F.s (Conf.dump t))
    | Some p ->
	begin try
	  let dl = Conf.dump ~prefix:(Conf.path_of_string p) t in
	  Excl (fun () -> F.s dl)
	with
	| Conf.Unbound (_, s) -> unknown_key_error s
	end
    end
  in
  make ?short ?long (`Any set)
    (F.x "dump a configuration key" [])
