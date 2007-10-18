let dev_null =
  Unix.openfile "/dev/null" [Unix.O_RDWR] 0

let play name =
  let sound_play = Data.get ["sound-play"] in
  let sound_file = Data.get ["sound"; name ^ ".wav"] in
  let _ =
    Unix.create_process sound_play [| sound_play; sound_file |]
      dev_null dev_null dev_null
  in
  ()
