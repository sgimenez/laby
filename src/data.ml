let log = Log.make ["data"]

let get ressource =
  let l =
    [
      Config.conf_path ^ ressource;
      Config.sys_data_path ^ ressource;
    ]
  in
  let error () =
    log#error (
      F.x "cannot find ressource <ressource> at: <list>" [
	"ressource", F.string ressource;
	"list", F.v (List.map F.string l);
      ]
    );
    Init.exit 1
  in
  begin try
    begin match List.filter Sys.file_exists l with
    | [] -> error ()
    | x :: _ -> x
    end
  with
  | Sys_error _ -> error ()
  end

let get_list dir =
  let f = Unix.opendir (get dir) in
  let list = ref [] in
  begin try
    while true do
      begin match Unix.readdir f with
      | "." | ".." -> ()
      | e -> list := e :: !list
      end
    done;
  with
  | End_of_file -> ()
  end;
  Unix.closedir f;
  !list
