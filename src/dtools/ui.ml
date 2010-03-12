(*
   Copyright (C) 2007-2010 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

let log = Log.make ["ui"]

let conf =
  Conf.void (F.x "display configuration" [])

let conf_texts =
  Conf.string ~p:(conf#plug "texts") ~d:"texts"
    (F.x "path for texts" [])

let conf_theme =
  Conf.string ~p:(conf#plug "theme") ~d:"theme"
    (F.x "path for theme" [])

let sys_lang =
  begin try Sys.getenv "LC_MESSAGES" with Not_found ->
    begin try Sys.getenv "LC_ALL" with Not_found ->
      begin try Sys.getenv "LANG" with Not_found ->
	""
      end
    end
  end

let conf_lang =
  Conf.string ~p:(conf#plug "lang") ~d:sys_lang
    (F.x "language" [])

let line_regexp =
  Str.regexp
    "^\\([-0-9a-zA-Z:_.]+\\)[ \t]+\\(.*\\)$"

let texts_line key s =
  begin match Str.string_match line_regexp s 0 with
  | true ->
      let val0 = Str.matched_group 1 s in
      let val1 = Str.matched_group 2 s in
      begin match String.sub val0 0 4 with
      | "text" ->
	  begin match String.length val0 with
	  | 4 -> key := ("", val1); `Skip
	  | l when val0.[4] == ':' ->
	      let special = String.sub val0 5 (l - 5) in
	      key := (special, val1); `Skip
	  | _ -> `Error
	  end
      | _ -> `Entry (val0, !key, val1)
      end
  | false ->
      if (s <> "\n" && s.[0] <> '#') then `Error else `Skip
  end

let read_texts path file =
  let lang = conf_lang#get in
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
  if conf_lang#get <> "" then
  begin try
    let path = Res.get [conf_texts#get] in
    Res.read_chan path (read_texts path)
  with
  | Res.Error msg ->
      log#warning msg
  end

let load_theme () =
  begin try
    let path = Res.get [conf_theme#get] in
    Conf.load ~log:(log#debug 2) Fd.conf_tags#ut path
  with
  | Res.Error msg ->
      log#warning msg
  end


let theme =
  Srv.make ~name:"load-theme" load_theme

let texts =
  Srv.make ~name:"load-texts" load_texts


