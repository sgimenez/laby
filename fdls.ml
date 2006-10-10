let print = F.print ~l:"fdls"

let conf_path = ref (Config.conf_path ^ "fdls.conf")
let log_path = ref (Config.conf_path ^ "fdls.log")
let texts_path = ref (Config.conf_path ^ "texts")
let theme_path = ref (Config.conf_path ^ "theme")

let proceed () =
  begin try
      Gfx.display_gtk State.next
    with
    | Gfx.Error ->
	print ~e:0 (fun () ->
	  F.text "display failed" []
	);
	Run.fail ()
  end

let main () =
  F.set_texts !texts_path;
  F.set_theme !theme_path;
  let log_opt = Opt.log_opt ~default:(Some (!log_path)) in
  let opts = [Version.opt; Opt.debug_opt; log_opt] in
  let arg s = () in
  begin try Opt.cmd opts arg proceed with
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
