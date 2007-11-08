let log = Log.make ["laby"]

let conf =
  Conf.void
    (F.x "laby configuration" [])

let conf_path = ref (Data.get ["conf"])
let texts_path = ref (Data.get ["texts"])
let theme_path = ref (Data.get ["theme"])

(* let conf_level = *)
(*   Conf.string ~p:(conf#plug "level") *)
(*     (F.x "level file" []) *)

(* let conf_prog = *)
(*   Conf.string ~p:(conf#plug "prog") *)
(*     (F.x "program file" []) *)

let proceed _ =
  begin try
    Gfx.display_gtk ()
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
(*   conf#plug "log" Log.conf#ut; *)
  let opts =
    [
      Version.opt;
(*       Conf.opt ~short:'p' ~long:"prog" conf_prog#ut; *)
(*       Conf.opt ~short:'l' ~long:"level" conf_level#ut; *)
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
      proceed list
  end

