
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let log = Log.make ["laby"]

let conf_path = ref (Data.get ["conf"])

let conf =
  Conf.root log !conf_path
    ~l:["log", Log.conf#ut; "display", Fd.conf#ut]
    (F.x "laby configuration" [])

let conf_lang =
  Conf.list ~p:(conf#plug "lang") ~d:[] (F.x "language" [])

let proceed _ =
  begin try
    Gfx.display_gtk ~language_list:conf_lang#get ()
  with
  | Gfx.Error f ->
      log#fatal (
	F.x "display failed: <error>" [
	  "error", f;
	]
      );
      Init.exit 1
  end

let main () =
  Fd.init log (Data.get []);
  let opts =
    [
      Version.opt;
      Opt.conf ~short:'l' ~long:"lang" conf_lang#ut;
      Opt.conf_set ~short:'c' ~long:"conf" conf;
      Opt.conf_descr ~long:"conf-descr" conf;
      Opt.conf_dump ~long:"conf-dump" conf;
      Opt.conf ~short:'d' ~long:"debug" Log.conf_level#ut;
    ]
  in
  begin match Opt.cmd opts with
  | `Errors ml ->
      log#fatal (
	F.x "invalid options: <errors>" [
	  "errors", F.v ml;
	]
      );
      Init.exit 1;
  | `Excl fn ->
      Fd.stdout (fn ())
  | `Proceed list ->
      proceed list
  end

let _ = Run.exec main
