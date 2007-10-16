let print = F.print ~l:"fdls"

let conf_path = ref (Config.conf_path ^ "fdls.conf")
let log_path = ref (Config.conf_path ^ "fdls.log")
let texts_path = ref (Config.conf_path ^ "texts")
let theme_path = ref (Config.conf_path ^ "theme")

let proceed _ =
  begin try
    Gfx.display_gtk Bot.caml
  with
  | Gfx.Error f ->
      print ~e:0 (
	F.x "display failed: <error>" [
	  "error", f;
	]
      );
      Run.fail ()
  end

let main () =
  F.set_texts !texts_path;
  F.set_theme !theme_path;
  let log_opt = Opt.log_opt ~default:(Some (!log_path)) in
  let opts = [Version.opt; Opt.debug_opt; log_opt] in
  begin try Opt.cmd opts proceed with
  | Opt.Error f ->
      print ~e:0 (
	F.x "incorrect options, <error>" [
	  "error", f;
	]
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
