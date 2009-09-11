
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let log = Log.make ["mod"]

let conf =
  Conf.void
    (F.x "mods configuration" [])

let conf_selected =
  Conf.string ~p:(conf#plug "selected") ~d:""
    (F.x "select a specific programming mod" [])

let check m =
  let command = Res.get ["scripts"; "mod"] in
  begin try
    let modlib = Res.get ["mods"; m; "lib"] in
    begin match flush_all (); Unix.fork () with
    | 0 ->
	Unix.execvp command [| command; modlib; "info" |]
    | pid ->
	let _, status = Unix.waitpid [] pid in
	status = Unix.WEXITED 0
    end
  with
  | Res.Error _ -> false
  end

let get_list () =
  begin match conf_selected#get with
  | "" ->
      List.filter check (Res.get_list ["mods"])
  | m ->
      if check m then [m]
      else
	Run.fatal (
	  F.x "selected mod cannot be found: <mod>" [
	    "mod", F.string m;
	  ]
	)
  end

type query = string * (string -> unit)

type t =
    <
      set_name: string -> unit;
      get_name: string;
      errto: (string -> unit) -> unit;
      set_buf: string -> unit;
      get_buf: string;
      start: bool;
      probe: query option;
      close: unit;
      help: string -> string;
    >

type h =
    {
      mutable close : unit -> unit;
      in_ch : Unix.file_descr;
      out_ch : Unix.file_descr;
      err_ch : Unix.file_descr;
      mutable current: string;
      mutable buf : string list;
    }

let bufsize = 16384
let buffer = String.create bufsize

let substs =
  [
    "laby_name_ant", F.xs "ascii" "ant" [];
    "laby_name_Ant", F.xs "ascii" "Ant" [];
    "laby_name_left", F.xs "ascii" "left" [];
    "laby_name_right", F.xs "ascii" "right" [];
    "laby_name_forward", F.xs "ascii" "forward" [];
    "laby_name_take", F.xs "ascii" "take" [];
    "laby_name_drop", F.xs "ascii" "drop" [];
    "laby_name_escape", F.xs "ascii" "escape" [];
    "laby_name_look", F.xs "ascii" "look" [];
    "laby_name_say", F.xs "ascii" "say" [];
    "laby_name_Void", F.xs "ascii" "Void" [];
    "laby_name_Wall", F.xs "ascii" "Wall" [];
    "laby_name_Rock", F.xs "ascii" "Rock" [];
    "laby_name_Web", F.xs "ascii" "Web" [];
    "laby_name_Exit", F.xs "ascii" "Exit" [];
    "laby_name_Unknown", F.xs "ascii" "Unknown" [];
  ]

let rec mktempdir ?(seed=0) ident =
  let pid = Unix.getpid () in
  let dir = Printf.sprintf "/tmp/%s-%d:%d/" ident pid seed in
  begin try Unix.mkdir dir 0o755; dir with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> mktempdir ~seed:(seed + 1) ident
  end

let dump prog =
  let tmpdir = mktempdir "ant" in
  let write filename s =
    let fd =
      Unix.openfile (tmpdir ^ filename)
	[ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY]
	0o644
    in
    ignore (Unix.write fd s 0 (String.length s));
    Unix.close fd;
  in
  write "program" prog;
  let subst =
    let f (x, y) = "s/" ^ x ^ "/" ^ Fd.render_raw y ^ "/g" in
    String.concat "\n" (List.map f substs);
  in
  write "subst" subst;
  tmpdir

let clean tmpdir =
  ignore (Unix.system ("rm -rf " ^ tmpdir))

let buf_read fd f =
  begin match Unix.read fd buffer 0 bufsize with
  | 0 -> false
  | i -> f (String.sub buffer 0 i); true
  end

let input ?(timeout=0.5) errto h =
  let output s =
    let str = s ^ "\n" in
    let len = String.length str in
    ignore (Unix.write h.out_ch str 0 len)
  in
  let collect s =
    for j = 0 to String.length s - 1; do
      begin match s.[j] with
      | '\n' ->
	  h.buf <- h.buf @ [h.current];
	  h.current <- "";
      | c -> h.current <- h.current ^ (String.make 1 c)
      end
    done
  in
  let rec loop () =
    begin match h.buf with
    | a :: q ->
	h.buf <- q; Some (a, output)
    | [] ->
	let l, _, _ = Unix.select [h.in_ch; h.err_ch] [] [] timeout in
	if l = [] then (errto "...\n"; None)
	else (
	  if List.mem h.err_ch l then
	    ignore (buf_read h.err_ch errto);
	  if List.mem h.in_ch l then
	    if buf_read h.in_ch collect
	    then loop ()
	    else None
	  else loop ()
	)
    end
  in
  loop ()

let make () =
  let subst s =
    let f s (x, y) =
      Str.global_replace (Str.regexp_string x) (Fd.render_raw y) s
    in
    List.fold_left f s substs
  in
  object (self)

    val hr = ref None

    val buffers = Hashtbl.create 64

    val errto = ref (fun s -> ())

    val name = ref "demo"

    method set_name n =
      name := n

    method get_name =
      !name

    method get_buf =
      begin try Hashtbl.find buffers !name with
      | Not_found ->
	  let s = ref "" in
	  let f = open_in (Res.get ["mods"; !name; "skel"]) in
	  begin try
	    while true do
	      s := !s ^ input_line f ^ "\n"
	    done
	  with
	  | End_of_file -> ()
	  end;
	  close_in f;
	  subst !s
      end

    method set_buf buf =
      Hashtbl.replace buffers !name buf

    method errto f =
      errto := f

    method start =
      self#close;
      let modlib = Res.get ["mods"; !name; "lib"] in
      let command = Res.get ["scripts"; "mod"] in
      let tmpdir = dump self#get_buf in
      let out_ch', out_ch = Unix.pipe () in
      let in_ch, in_ch' = Unix.pipe () in
      let err_ch, err_ch' = Unix.pipe () in
      let prep () =
	begin match flush_all (); Unix.fork () with
	| 0 ->
	    Unix.chdir tmpdir;
	    Unix.execvp command [| command; modlib; "prep" |]
	| pid ->
	    let _, status = Unix.waitpid [] pid in
	    status = Unix.WEXITED 0
	end
      in
      let run () =
	begin match flush_all (); Unix.fork () with
	| 0 ->
	    Unix.dup2 in_ch' Unix.stdout;
	    Unix.dup2 out_ch' Unix.stdin;
	    Unix.dup2 err_ch' Unix.stderr;
	    Unix.close in_ch; Unix.close out_ch; Unix.close err_ch;
	    Unix.chdir tmpdir;
	    begin try
	      Unix.execvp command [| command; modlib; "run" |]
	    with
	    | exn ->
		log#error (
		  F.x "execution of interpreter failed" []
		);
		assert false
	    end
	| pid ->
	    Unix.close in_ch'; Unix.close out_ch'; Unix.close err_ch';
	    let close () =
	      Unix.close in_ch; Unix.close out_ch; Unix.close err_ch;
	      clean tmpdir;
	      Unix.kill pid Sys.sigterm;
	      ignore (Unix.waitpid [] pid)
	    in
	    let h =
	      {
		close = close;
		in_ch = in_ch; out_ch = out_ch; err_ch = err_ch;
		current = ""; buf = [];
	      }
	    in
	    begin match input ~timeout:5. !errto h with
	    | Some ("start", output) ->
		output "go";
		hr := Some h;
		true
	    | _ ->
		false
	    end
	end
      in
      prep () && run ()

    method close =
      begin match !hr with
      | None -> ()
      | Some h -> h.close (); hr := None
      end

    method probe =
      begin match !hr with
      | None -> None
      | Some h -> input !errto h
      end

    method help s =
      let mf =
	begin try
	  Some (open_in (Res.get ["mods"; !name; "help"]))
	with
	| Res.Error _ -> None
	end
      in
      let rec input f blocks lines =
	begin try
	  begin match input_line f with
	  | "" -> input f (List.rev lines :: blocks) []
	  | l -> input f blocks (l :: lines)
	  end
	with
	| End_of_file -> List.rev (List.rev lines :: blocks)
	end
      in
      let rec app s f blocks =
	begin match blocks with
	| [] -> ""
	| (h :: lines) :: q ->
	    if h = s then f lines else app s f q
	| [] :: q -> ""
	end
      in
      begin match mf with
      | None -> ""
      | Some f ->
	  let blocks = input f [] [] in
	  close_in f;
	  subst (app (s ^ ":") (String.concat "\n") blocks) ^ "\n"
      end

  end
