let log = Log.make ["ui"]

let conf =
  Conf.void (F.x "display configuration" [])

let conf_texts =
  Conf.string ~p:(conf#plug "texts") ~d:"texts"
    (F.x "path for texts" [])

let conf_theme =
  Conf.string ~p:(conf#plug "theme") ~d:"theme"
    (F.x "path for theme" [])

let lang =
  begin try Sys.getenv "LC_MESSAGES" with Not_found ->
    begin try Sys.getenv "LC_ALL" with Not_found ->
      begin try Sys.getenv "LANG" with Not_found ->
	""
      end
    end
  end

let texts_line key str =
  let msg_lang = ref "" in
  let special = ref "" in
  let txt = ref "" in
  let msg = ref "" in
  let push c str = str := !str ^ String.make 1 c in
  let p = ref 0 in
  let eos = String.length str in
  let state = ref `Init in
  while !p < eos do
    state :=
      begin match !state, str.[!p] with
      | `Init, '\t' ->
	  while str.[!p] = '\t' do incr p done;
	  if !msg_lang = "text"
	  then `Text
	  else `Msg
      | `Init, ':' ->
	  if !msg_lang <> "text"
	  then `Error
	  else begin
	    incr p;
	    let i = !p in
	    while str.[!p] <> '\t' do incr p done;
	    special := String.sub str i (!p - i);
	    while str.[!p] = '\t' do incr p done;
	    `Text
	  end
      | `Init, ' ' -> `Error
      | `Init, c -> push c msg_lang; incr p; `Init
      | `Text, '\n' -> incr p; `Text
      | `Text, c -> incr p; push c txt; `Text
      | `Msg, c -> push c msg; incr p; `Msg
      | `Error, _ -> p := eos; `Error
      end
  done;
  begin match !state with
  | `Text -> key := (!special, !txt); `Skip
  | `Msg -> `Entry (!msg_lang, !key, !msg)
  | _ ->
      if (str <> "\n" && str.[0] <> '#') then `Error else `Skip
  end

let read_texts path file =
  let error line f =
    log#error (
      F.x "file <f>, line <l>: <error>" [
	"f", F.string path;
	"l", F.int line;
	"error", f
      ]
    )
  in
  let lnb = ref 0 in
  begin try
    let k = ref ("", "") in
    while true do
      let str = incr lnb; input_line file ^ "\n" in
      begin match texts_line k str with
      | `Entry (msg_lang, key, msg) ->
	  log#debug 5 (
	    F.x "message special=<id> lang=<lang>: <text>" [
	      "id", F.string (fst key);
	      "lang", F.string msg_lang;
	      "text", F.q (F.s (snd key));
	    ]
	  );
	  if lang = msg_lang then Fd.def_text key msg
      | `Skip -> ()
      | `Error -> error !lnb (F.x "syntax error" [])
      end
    done
  with
  | End_of_file -> ()
  end


let load_texts () =
  begin try
    Res.use [conf_texts#get] (
      fun filename file ->
	read_texts filename file;
    )
  with
  | Res.Error msg ->
      log#warning msg
  end

let load_theme () =
  begin try
    let filename = Res.get [conf_theme#get] in
    Conf.load ~log:(log#debug 2) Fd.conf_tags#ut filename
  with
  | Res.Error msg ->
      log#warning msg
  end


let theme =
  Init.make ~name:"load-theme" load_theme

let texts =
  Init.make ~name:"load-texts" load_texts


