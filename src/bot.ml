let log = Log.make ["bot"]

type query = string * (string -> unit)

type t =
    <
      set: string -> unit;
      errto: (string -> unit) -> unit;
      skel: string;
      lang_file: string;
      start: string -> unit;
      probe: query option;
      close: unit;
    >

type h =
    {
      pid: int;
      filename: string;
      robot: string;
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

let dump prog =
  let filename = Printf.sprintf "/tmp/fourmi-%d.ml" (Unix.getpid ()) in
  let fd =
    Unix.openfile filename
      [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY]
      0o644
  in
  ignore (Unix.write fd prog 0 (String.length prog));
  Unix.close fd;
  filename

let exec_caml dir h =
  begin try
      Unix.execvp "ocaml" [| "ocaml"; h.filename |]
    with
      exn ->
	log#error (
	  F.x "execution of program failed" []
	);
  end


let input errto h =
  let output s =
    Printf.fprintf (Unix.out_channel_of_descr (h.out_ch)) "%s\n%!" s
  in
  let rec read () =
    begin match h.buf with
    | a :: q ->
	h.buf <- q; Some (a, output)
    | [] ->
	let l, _, _ = Unix.select [h.in_ch; h.err_ch] [] [] 0.5 in
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

    val errto = ref (fun s -> ())

    val dir = ref "default/"

    method set name =
      dir := Data.get "run/" ^ name ^ "/"

    method skel =
      let s = ref "" in
      let f = open_in (!dir ^ "skel") in
      begin try
	  while true do
	    s := !s ^ input_line f ^ "\n"
	  done
	with
	| End_of_file -> ()
      end;
      close_in f;
      !s

    method lang_file =
      !dir ^ "lang"

    method errto f =
      errto := f

    method start prog =
      self#close;
      let slave h =
	Unix.chdir !dir;
	begin try
	    Unix.execvp "./command" [| "./command"; h.filename |]
	  with
	    exn ->
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
	  filename = dump prog;
	  robot = prog;
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
	  begin match input !errto h with
	  | Some ("start", output) ->
	      output "go";
	      !errto "Robot has started\n";
	      hr := Some { h with pid = pid }
	  | _ ->
	      !errto "Robot has not started\n"
	  end
      end

    method close =
      begin match !hr with
      | None -> ()
      | Some h ->
	  Unix.close h.in_ch;
	  Unix.close h.out_ch;
	  Unix.close h.err_ch;
	  Unix.unlink h.filename;
	  ignore (Unix.waitpid [] h.pid);
	  hr := None
      end

    method probe =
      begin match !hr with
      | None -> None
      | Some h -> input !errto h
      end

  end
