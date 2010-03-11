
(*
 * Copyright (C) 2007-2010 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let conf =
  Conf.void
    ~l:[
      "mod", Mod.conf#ut;
      "log", Log.conf#ut;
      "ui", Ui.conf#ut;
      "gfx", Gfx.conf#ut;
      "res", Res.conf#ut;
      "sound", Sound.conf#ut;
    ]
    (F.x "laby configuration" [])

let proceed _ =
  begin try
    Gfx.display_gtk ()
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
    Opt.conf ~short:'m' ~long:"mod" Mod.conf_selected;
    Opt.conf ~short:'s' ~long:"sound" Sound.conf_enabled;
    Opt.conf ~long:"tile-size" Gfx.conf_tilesize;
    Opt.conf_set ~short:'c' ~long:"conf" conf;
    Opt.conf_descr ~long:"conf-descr" conf;
    Opt.conf_dump ~long:"conf-dump" conf;
    Run.opt_debug;
  ]

let _ =
  Run.init
    ~name:Config.project_name
    ~conf:(conf, ["conf"])
    ~services:[Ui.theme; Ui.texts]
    (`Opts (opts, proceed))
