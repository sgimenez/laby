let conf =
  Conf.void
    ~l:[
      "log", Log.conf#ut;
      "ui", Ui.conf#ut;
      "res", Res.conf#ut;
    ]
    (F.x "laby configuration" [])

let conf_lang =
  Conf.list ~p:(conf#plug "lang") ~d:[] (F.x "language" [])

let proceed _ =
  begin try
    Gfx.display_gtk ~language_list:conf_lang#get ()
  with
  | Gfx.Error f ->
      Run.fatal (
	F.x "display failed: <error>" [
	  "error", f;
	]
      )
  end

let opts =
  [
    Version.opt;
    Opt.conf ~short:'l' ~long:"lang" conf_lang;
    Opt.conf_set ~short:'c' ~long:"conf" conf;
    Opt.conf_descr ~long:"conf-descr" conf;
    Opt.conf_dump ~long:"conf-dump" conf;
    Opt.conf ~short:'d' ~long:"debug" Log.conf_level;
  ]

let _ =
  Run.init
    ~path:[Config.conf_path; Config.sys_data_path;]
    ~conf:(conf, ["conf"])
    ~services:[Ui.theme; Ui.texts]
    (`Opts (opts, proceed))
