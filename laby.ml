let print = F.print ~l:"fdls"

let conf_path = ref (Config.conf_path ^ "fdls.conf")
let log_path = ref (Config.conf_path ^ "fdls.log")
let texts_path = ref (Config.conf_path ^ "texts")
let theme_path = ref (Config.conf_path ^ "theme")

let map = ref ""

let level_opt =
  let handle_s level =
    let file = Config.conf_path ^ Printf.sprintf "levels/" ^ level in
    begin match Sys.file_exists file with
    | true -> map := file; Opt.Noop
    | false ->
	print ~e:0 (fun () ->
	  F.text "level <level> does not exists" [
	      "level", F.sq level;
	  ]
	);
	Run.fail ()
    end
  in
  'L', "level", None, Some handle_s,
  F.s "chooses level file"

let launch robot () =
  let in_ch, in_ch' = Unix.pipe () in
  let out_ch', out_ch = Unix.pipe () in
  begin match flush_all (); Unix.fork () with
  | 0 ->
      Unix.dup2 in_ch' Unix.stdout;
      Unix.dup2 out_ch' Unix.stdin;
      Unix.close in_ch;
      Unix.close out_ch;
      begin try
	  Unix.execv robot [| robot |]
	with
	  exn ->
	    print ~e:0 (fun () ->
	      F.text "execution of <program> failed" [
		  "program", F.sq robot;
	      ]
	    );
      end;
      assert false
  | pid ->
      Unix.close in_ch';
      Unix.close out_ch';
      let rec input =
	let buf = String.make 1024 ' ' in
	let current = ref "" in
	let buffer = ref [] in
	begin fun () ->
	  begin match !buffer with
	  | a :: q -> buffer := q; Some a
	  | [] ->
	      let l, _, _ = Unix.select [in_ch] [] [] 0.2 in
	      begin match l with
	      | [] -> None
	      | _ ->
		  begin try
		      begin match Unix.read (in_ch) buf 0 1024 with
		      | 0 -> None
		      | i ->
			  for j = 0 to i - 1; do
			    begin match buf.[j] with
			    | '\n' ->
				buffer := !buffer @ [!current];
				current := ""
			    | c -> current := !current ^ (String.make 1 c)
			    end
			  done;
			  input ()
		      end
		    with
		    | End_of_file -> None
		  end
	      end
	  end
	end
      in
      let output s =
	Printf.fprintf (Unix.out_channel_of_descr (out_ch)) "%s\n%!" s
      in
      let close () =
	Unix.close in_ch;
	Unix.close out_ch;
	ignore (Unix.waitpid [] pid)
      in
      input, output, close
  end

let display robot =
  begin try
      Gfx.display_gtk !map (launch robot)
    with
    | Gfx.Error ->
	print ~e:0 (fun () ->
	  F.text "display failed" []
	);
	Run.fail ()
  end

let proceed robots =
  begin match robots with
  | [robot] ->
      display robot
  | [] ->
      print ~e:0 (fun () ->
	F.text "no robot specified" []
      );
      Run.fail ()
  | _ ->
      print ~e:0 (fun () ->
	F.text "can't handle more than one robot for now" []
      );
      Run.fail ()
  end

let main () =
  F.set_texts !texts_path;
  F.set_theme !theme_path;
  let log_opt = Opt.log_opt ~default:(Some (!log_path)) in
  let opts = [Version.opt; Opt.debug_opt; log_opt; level_opt] in
  begin try Opt.cmd opts proceed with
  | Opt.Error ->
      print ~e:0 (fun () ->
	F.text "incorrect options" []
      );
      Run.fail ()
  end

let _ =
  Run.exec (fun () ->
    begin match Run.run main with
    | Run.Done () -> exit 0
    | Run.Exited -> exit 0
    | Run.Failed -> exit 1
    | Run.Exn exn -> F.exn exn
    end
  )
