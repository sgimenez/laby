let log = Log.make ["laby"]

let conf_path = ref (Data.get ["conf"])

let conf =
  Conf.root log !conf_path
    ~l:["log", Log.conf#ut; "display", Fd.conf#ut]
    (F.x "laby configuration" [])

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

let main () =
  Fd.init log (Data.get []);
  let opts =
    [
      Version.opt;
      Opt.conf ~short:'d' ~long:"debug" Log.conf_level#ut;
      Opt.conf_set ~short:'c' ~long:"conf" conf;
      Opt.conf_descr ~long:"conf-descr" conf;
      Opt.conf_dump ~long:"conf-dump" conf;
(*       Opt.conf ~short:'p' ~long:"prog" conf_prog#ut; *)
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

let _ = Run.exec main
