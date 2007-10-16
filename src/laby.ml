let log = Log.make ["laby"]

let conf =
  Conf.void
    (F.x "laby configuration" [])

let conf_path = ref (Config.conf_path ^ "fdls.conf")
let log_path = ref (Config.conf_path ^ "fdls.log")
let texts_path = ref (Config.conf_path ^ "texts")
let theme_path = ref (Config.conf_path ^ "theme")

let conf_map =
  Conf.string ~p:(conf#plug "map")
    (F.x "map file" [])

let proceed _ =
  begin try
    Gfx.display_gtk Bot.caml
  with
  | Gfx.Error f ->
      log#fatal (
	F.x "display failed: <error>" [
	  "error", f;
	]
      );
      Init.exit 1
  end

let main =
  Fd.set_texts !texts_path;
  conf#plug "log" Log.conf#ut;
  let opts =
    [
      Version.opt;
      Conf.opt ~short:'L' ~long:"level" conf_map#ut;
      Conf.opt_descr ~long:"conf-descr" conf;
    ]
  in
  begin match Opt.cmd opts with
  | `Errors ml ->
      log#fatal (
	F.x "incorrect options: <errors>" [
	  "errors", F.v ml;
	]
      );
      Init.exit 1;
  | `Excl fn ->
      Fd.stdout (fn ())
  | `Proceed list ->
      Init.init (fun () -> proceed list)
  end

