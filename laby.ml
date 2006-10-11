let print = F.print ~l:"fdls"

let conf_path = ref (Config.conf_path ^ "fdls.conf")
let log_path = ref (Config.conf_path ^ "fdls.log")
let texts_path = ref (Config.conf_path ^ "texts")
let theme_path = ref (Config.conf_path ^ "theme")

let proceed robots =
  begin match robots with
  | [robot] ->
      let in_ch, in_ch' = Unix.pipe () in
      let out_ch', out_ch = Unix.pipe () in
      begin match Unix.fork () with
      | 0 ->
	  Unix.dup2 in_ch' Unix.stdout;
	  Unix.dup2 out_ch' Unix.stdin;
	  Unix.close in_ch;
	  Unix.close out_ch;
	  begin try
	      Unix.execv Sys.argv.(1) [||]
	    with
	      exn ->
		print ~e:0 (fun () ->
		  F.text "execution of <program> failed" [
		      "program", F.sq Sys.argv.(1);
		  ]
		);
	  end
      | pid ->
	  Unix.close in_ch';
	  Unix.close out_ch';
	  let next = State.run (in_ch, out_ch) in
	  begin try
	      Gfx.display_gtk next
	    with
	    | Gfx.Error ->
		print ~e:0 (fun () ->
		  F.text "display failed" []
		);
		Run.fail ()
	  end
      end
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
  let opts = [Version.opt; Opt.debug_opt; log_opt] in
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
