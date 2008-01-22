(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

type link = string
type path = link list

type ut =
    <
      kind: string option;
      descr: F.t;
      comments: F.t list;
      plug: link -> ut -> unit;
      subs: link list;
      path: path -> ut;
      routes: ut -> path list;
      ut: ut;
    >

type 'a t =
    <
      kind: string option;
      descr: F.t;
      comments: F.t list;
      plug: string -> ut -> unit;
      subs: string list;
      path: string list -> ut;
      routes: ut -> path list;
      ut: ut;
      set_d: 'a option -> unit;
      get_d: 'a option;
      set: 'a -> unit;
      get: 'a;
    >

type links = (string * ut) list

type 'a builder =
    ?d:'a -> ?p:(ut -> unit) -> ?l:links -> ?comments:F.t list -> F.t -> 'a t

exception Undefined of ut
exception Invalid of string
exception Unbound of ut * string
exception Bound of ut * string
exception Mismatch of ut
exception Wrong_Conf of string * string
exception File_Wrong_Conf of string * int * string
exception Cyclic of ut * ut

let path_sep_regexp =
  Str.regexp "\\."
let list_sep_regexp =
  Str.regexp ":"
let line_regexp =
  Str.regexp
    "^[ \t]*\\([a-zA-Z]+\\)[ \t]+\\([a-zA-Z0-9._-]+\\)[ \t]*:\\(.*\\)$"
let comment_regexp =
  Str.regexp "^[ ]*\\(#.*\\)?$"

let check s =
  if Str.string_match path_sep_regexp s 0 then raise (Invalid (s))

let make kind
    ?(d : 'a option)
    ?(p : ut -> unit = fun _ -> ())
    ?(l : links = [])
    ?(comments : F.t list = [])
    descr
    : 'a t =
object (self)

  val kind : string option = kind
  val descr : F.t = descr
  val comments : F.t list = comments

  val mutable links : links = []

  val mutable value_d : 'a option = d
  val mutable value : 'a option = None

  initializer
    p self#ut;
    List.iter (fun (s, t) -> self#plug s t) l

  method subs =
    List.sort compare (List.map fst links)

  method private sub (s : string) : ut =
    check s;
    begin try
	List.assoc s links
      with
      | Not_found -> raise (Unbound (self#ut, s))
    end

  method path (l : string list) : ut =
    begin match l with
    | [] -> self#ut
    | s :: q -> (self#sub s)#path q
    end

  method routes (st : ut) =
    (* todo: cache already accessed nodes *)
    let rec aux l t =
      begin match t = st with
      | true -> [List.rev l]
      | false ->
	  List.concat (List.map (fun s -> aux (s :: l) (t#path [s])) t#subs)
      end
    in
    aux [] self#ut

  method kind = kind

  method descr = descr
  method comments = comments

  method plug s t =
    if t#routes self#ut <> [] then raise (Cyclic (self#ut, t));
    if List.mem_assoc s links then raise (Bound (self#ut, s));
    links <- (s, t) :: links

  method ut = (self :> ut)

  method get_d : 'a option = value_d

  method set_d (v : 'a option) : unit = value_d <- v

  method get : 'a =
    begin match value with
    | None ->
	begin match value_d with
	| None -> raise (Undefined (self#ut))
	| Some v -> v
	end
    | Some v -> v
    end

  method set (v : 'a) : unit = value <- Some v

end

let void ?p ?l ?comments descr =
  (make None ?p ?l ~d:None ?comments descr)#ut

let unit ?d = make (Some "unit") ?d
let int ?d = make (Some "int") ?d
let float ?d = make (Some "float") ?d
let bool ?d = make (Some "bool") ?d
let string ?d = make (Some "string") ?d
let list ?d = make (Some "list") ?d

(* Harmful function, do not use *)
let force_type c t : 'a t =
  begin match t#kind with
  | Some x when x = c -> (Obj.magic t : 'a t)
  | _ -> raise (Mismatch (t#ut))
  end

let as_unit t : unit t = force_type "unit" t
let as_int t : int t = force_type "int" t
let as_float t : float t = force_type "float" t
let as_bool t : bool t = force_type "bool" t
let as_string t : string t = force_type "string" t
let as_list t : string list t = force_type "list" t

let path_of_string p =
  Str.split path_sep_regexp p
let string_of_path p =
  String.concat "." p

let routes (t : ut) (st : ut) =
  let rec aux l t =
    begin match t = st with
    | true -> [List.rev l]
    | false ->
	List.concat (List.map (fun s -> aux (s :: l) (t#path [s])) t#subs)
    end
  in
  aux [] t

let get_string (t : ut) =
  begin try
      begin match t#kind with
      | None -> None
      | Some "unit" -> Some ("")
      | Some "int" -> Some (string_of_int (as_int t)#get)
      | Some "float" -> Some (string_of_float (as_float t)#get)
      | Some "bool" -> Some (string_of_bool (as_bool t)#get)
      | Some "string" -> Some ((as_string t)#get)
      | Some "list" -> Some (String.concat ":" (as_list t)#get)
      | _ -> assert false
      end
    with
    | Undefined _ -> None
  end

let get_d_string (t : ut) =
  let mapopt f = (function None -> None | Some x -> Some (f x)) in
  begin try
      begin match t#kind with
      | None -> None
      | Some "unit" -> mapopt (fun () -> "") (as_unit t)#get_d
      | Some "int" -> mapopt string_of_int (as_int t)#get_d
      | Some "float" -> mapopt string_of_float (as_float t)#get_d
      | Some "bool" -> mapopt string_of_bool (as_bool t)#get_d
      | Some "string" -> (as_string t)#get_d
      | Some "list" -> mapopt (String.concat ":") (as_list t)#get_d
      | _ -> assert false
      end
    with
    | Undefined _ -> None
  end

let descr ?(prefix=[]) (t : ut) =
  let rec aux prefix t =
    let p s = if prefix = "" then s else prefix ^ "." ^ s in
    let subs =
      List.concat (List.map (function s -> aux (p s) (t#path [s])) t#subs)
    in
    let title =
      [F.h [F.s "## "; t#descr]]
    in
    let default =
      begin match get_d_string t with
      | None -> []
      | Some d -> [F.h [F.s "# default :"; F.s d]]
      end
    in
    let line =
      begin match t#kind, get_string t with
      | Some k, None ->
	  [F.s (Printf.sprintf "#%s\t%-30s" k prefix)]
      | Some k, Some p ->
	  [F.s (Printf.sprintf "%s\t%-30s :%s" k prefix p)]
      | _ -> []
      end
    in
    let comments =
      begin match t#comments with
      | [] -> []
      | l ->
	  [
	    F.v ~head:(F.s "# ") [
	      F.s "comments:";
	      F.v l
	    ]
	  ]
      end
    in
    F.v ~head:F.n (title @ default @ line @ comments) :: subs
  in
  aux (string_of_path prefix) (t#path prefix)

let dump ?(prefix=[]) (t : ut) =
  let rec aux prefix t =
    let p s = if prefix = "" then s else prefix ^ "." ^ s in
    let subs =
      List.map (function s -> aux (p s) (t#path [s])) t#subs
    in
    begin match t#kind with
    | Some k ->
	begin match get_d_string t, get_string t with
	| None, None ->
	    Printf.sprintf "#%s\t%-30s\n" k prefix
	| Some p, None ->
	    Printf.sprintf "#%s\t%-30s :%s\n" k prefix p
	| Some p, Some p' when p' = p ->
	    Printf.sprintf "#%s\t%-30s :%s\n" k prefix p
	| _, Some p ->
	    Printf.sprintf "%s\t%-30s :%s\n" k prefix p
	end
    | _ -> ""
    end ^
      String.concat "" subs
  in
  aux (string_of_path prefix) (t#path prefix)

let conf_set (t: ut) s =
  if Str.string_match line_regexp s 0
  then
    let val0 = Str.matched_group 1 s in
    let val1 = Str.matched_group 2 s in
    let val2 = Str.matched_group 3 s in
    let st = t#path (path_of_string val1) in
    begin match val0 with
    | "unit" ->
	begin match val2 = "" with
	| false -> raise (Wrong_Conf (s, "unit expected"))
	| true -> (as_unit st)#set ()
	end
    | "int" ->
	let i =
	  begin try int_of_string val2  with
	  | Invalid_argument _ ->
	      raise (Wrong_Conf (s, "integer expected"))
	  end
	in
	(as_int st)#set i
    | "float" ->
	let f =
	  begin try float_of_string val2 with
	  | Invalid_argument _ ->
	      raise (Wrong_Conf (s, "float expected"))
	  end
	in
	(as_float st)#set f
    | "bool" ->
	let b =
	  begin try bool_of_string val2 with
	  | Invalid_argument _ ->
	      raise (Wrong_Conf (s, "boolean expected"))
	  end
	in
	(as_bool st)#set b
    | "string" ->
	let s = val2 in
	(as_string st)#set s
    | "list" ->
	let l = Str.split list_sep_regexp val2 in
	(as_list st)#set l
    | _ -> raise (Wrong_Conf (s, "unknown type"))
    end
  else raise (Wrong_Conf (s, "syntax error"))

let conf_file ?(strict=true) t s =
  let nb = Pervasives.ref 0 in
  let f = open_in s in
  begin try
      while true do
	nb := !nb + 1;
	let l = input_line f in
	if Str.string_match comment_regexp l 0
	then ()
	else
	  begin try conf_set t l with
	  | Wrong_Conf (x,y) ->
	      raise (File_Wrong_Conf (s,!nb,y))
	  | Unbound (e, p) ->
	      if strict then raise (Unbound (e, p))
	  end
      done
    with
    | End_of_file -> ()
  end

