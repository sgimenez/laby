let log = Log.make ["data"]

let get ressource =
  let l =
    [
      Config.conf_path ^ ressource;
      Config.sys_data_path ^ ressource;
    ]
  in
  begin match List.filter Sys.file_exists l with
  | [] ->
      log#error (
	F.x "cannot find ressource <ressource> at: <list>" [
	  "ressource", F.string ressource;
	  "list", F.v (List.map F.string l);
	]
      );
      Init.exit 1
  | x :: _ ->
      x
  end
