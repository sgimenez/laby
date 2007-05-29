let dev_null =
  Unix.openfile "/dev/null" [Unix.O_RDWR] 0

let play n =
  let sound_play = Config.conf_path ^ "sound-play" in
  let sound_file = Printf.sprintf "%ssound/%s.wav" Config.conf_path n in
  let _ =
    Unix.create_process sound_play [| sound_play; sound_file |]
      dev_null dev_null dev_null
  in
  ()
