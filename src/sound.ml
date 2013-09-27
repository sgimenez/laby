
(*
 * Copyright (C) 2007-2010 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let log = Log.make ["sound"]

let conf =
  Conf.void
    (F.x "sound configuration" [])

let conf_enabled =
  Conf.bool ~p:(conf#plug "enabled") ~d:(Sys.os_type <> "Win32")
    (F.x "enable or disable sounds" [])

let conf_script =
  Conf.string ~p:(conf#plug "script") ~d:"sound"
    (F.x "script to play sound files" [])

let play name =
  let script = conf_script#get in
  if conf_enabled#get && script <> "" then
    let path = Res.get ["scripts"; script] in
    let sound_file = Res.get ["sound"; name ^ ".wav"] in
    let i_r, i_w = Unix.pipe () in
    let o_r, o_w = Unix.pipe () in
    begin try
      let pid =
	Unix.create_process path [| script; sound_file |] i_r o_w o_w
      in
      ignore (Unix.waitpid [] pid);
    with
    | Unix.Unix_error (_, _, _) ->
	log#warning (
	  F.x "failed to play sound file using <name> script" [
	    "name", F.string script;
	  ]
	)
    end;
    Unix.close i_r; Unix.close i_w; Unix.close o_r; Unix.close o_w

let action =
  begin function
  | `None -> ()
  | `Start -> play "start"
  | `Wall_In -> play "wall-in"
  | `Rock_In -> play "rock-in"
  | `Exit_In -> play "exit-in"
  | `Web_In -> play "web-in"
  | `Web_Out -> play "web-out"
  | `Exit -> play "exit"
  | `No_Exit -> play "no-exit"
  | `Carry_Exit -> play "carry-exit"
  | `Rock_Take -> play "rock-take"
  | `Rock_Drop -> play "rock-drop"
  | `Take_Nothing -> play "take-nothing"
  | `Take_No_Space -> play "take-no-space"
  | `Drop_Nothing -> play "drop-nothing"
  | `Drop_No_Space -> play "drop-no-space"
  | `Say _ -> ()
  end

