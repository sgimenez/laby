
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let log = Log.make ["bot"]

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
    >

type h =
    {
      pid: int;
      tmpdir: string;
      in_ch : Unix.file_descr;
      in_ch' : Unix.file_descr;
      out_ch : Unix.file_descr;
      out_ch' : Unix.file_descr;
      err_ch : Unix.file_descr;
      err_ch' : Unix.file_descr;
      mutable buf_in : string;
      mutable buf_err: string;
      mutable current: string;
      mutable buf : string list;
    }

let bufsize = 16384

let substs =
  [
    "laby_name_ant", F.xs "ascii" "ant" [];
    "laby_name_Ant", F.xs "ascii" "Ant" [];
    "laby_name_left", F.xs "ascii" "left" [];
    "laby_name_right", F.xs "ascii" "right" [];
    "laby_name_forward", F.xs "ascii" "forward" [];
    "laby_name_look", F.xs "ascii" "look" [];
    "laby_name_door_open", F.xs "ascii" "door_open" [];
    "laby_name_take", F.xs "ascii" "take" [];
    "laby_name_drop", F.xs "ascii" "drop" [];
    "laby_name_Void", F.xs "ascii" "Void" [];
    "laby_name_Wall", F.xs "ascii" "Wall" [];
    "laby_name_Rock", F.xs "ascii" "Rock" [];
    "laby_name_Web", F.xs "ascii" "Web" [];
    "laby_name_Exit", F.xs "ascii" "Exit" [];
  ]

let dump prog =
  let tmpdir = Printf.sprintf "/tmp/fourmi-%d/" (Unix.getpid ()) in
  ignore (Unix.system ("rm -rf " ^ tmpdir));
  Unix.mkdir tmpdir 0o755;
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

let input ?(timeout=0.5) errto h =
  let output s =
    Printf.fprintf (Unix.out_channel_of_descr (h.out_ch)) "%s\n%!" s
  in
  let rec read () =
    begin match h.buf with
    | a :: q ->
	h.buf <- q; Some (a, output)
    | [] ->
	let l, _, _ = Unix.select [h.in_ch; h.err_ch] [] [] timeout in
	if List.mem h.err_ch l then
	  begin
	    let r = ref 1 in
	    while !r <> 0 do
	      r := Unix.read h.err_ch h.buf_err 0 bufsize;
	      errto (String.sub h.buf_err 0 !r)
	    done;
	  end;
	if List.mem h.in_ch l then
	  begin
	    begin match Unix.read h.in_ch h.buf_in 0 bufsize with
	    | 0 -> None
	    | i ->
		for j = 0 to i - 1; do
		  begin match h.buf_in.[j] with
		  | '\n' ->
		      h.buf <- h.buf @ [h.current];
		      h.current <- ""
		  | c -> h.current <- h.current ^ (String.make 1 c)
		  end
		done;
		read ();
	    end
	  end
	else None
    end
  in
  read ()

let make () =
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
      let subst s =
	let f s (x, y) =
	  Str.global_replace (Str.regexp_string x) (Fd.render_raw y) s
	in
	List.fold_left f s substs
      in
      begin try Hashtbl.find buffers !name with
      | Not_found ->
	  let s = ref "" in
	  let f = open_in (Res.get ["run"; !name; "skel"]) in
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
      let slave h =
	Unix.chdir (Res.get ["run"; !name]);
	begin try
	  Unix.execvp "./command" [| "./command"; h.tmpdir |]
	with
	| exn ->
	    log#error (
	      F.x "execution of interpreter failed" []
	    );
	end
      in
      let in_ch, in_ch' = Unix.pipe () in
      let out_ch', out_ch = Unix.pipe () in
      let err_ch, err_ch' = Unix.pipe () in
      let h =
	{
	  pid = 0;
	  tmpdir = dump self#get_buf;
	  in_ch = in_ch; in_ch' = in_ch';
	  out_ch = out_ch; out_ch' = out_ch';
	  err_ch = err_ch; err_ch' = err_ch';
	  buf_in = String.make bufsize ' ';
	  buf_err = String.make bufsize ' ';
	  current = "";
	  buf = [];
	}
      in
      begin match flush_all (); Unix.fork () with
      | 0 ->
	  Unix.dup2 h.in_ch' Unix.stdout;
	  Unix.dup2 h.out_ch' Unix.stdin;
	  Unix.dup2 h.err_ch' Unix.stderr;
	  Unix.close h.in_ch;
	  Unix.close h.out_ch;
	  Unix.close h.err_ch;
	  slave h;
	  assert false
      | pid ->
	  Unix.close in_ch';
	  Unix.close out_ch';
	  Unix.close err_ch';
	  begin match input ~timeout:5. !errto h with
	  | Some ("start", output) ->
	      output "go";
	      hr := Some { h with pid = pid };
	      true
	  | _ ->
	      false
	  end
      end

    method close =
      begin match !hr with
      | None -> ()
      | Some h ->
	  Unix.close h.in_ch;
	  Unix.close h.out_ch;
	  Unix.close h.err_ch;
	  clean h.tmpdir;
	  Unix.kill h.pid Sys.sigterm;
	  ignore (Unix.waitpid [] h.pid);
	  hr := None
      end

    method probe =
      begin match !hr with
      | None -> None
      | Some h -> input !errto h
      end

  end
