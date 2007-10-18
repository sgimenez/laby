let log = Log.make ["data"]

let get ressource =
  let check f =
    begin try Sys.file_exists f with
    | Sys_error _ ->
	log#warning (
	  F.x "cannot access <path>" [
	    "path", F.string f;
	  ]
	);
	false
    end
  in
  let dir_list =
    List.filter check
      [ Config.conf_path; Config.sys_data_path; ]
  in
  let l = List.map (fun s -> s ^ ressource) dir_list in
  let error () =
    log#error (
      F.x "cannot find ressource <ressource> at: <list>" [
	"ressource", F.string ressource;
	"list", F.v (List.map F.string l);
      ]
    );
    Init.exit 1
  in
  let rec may =
    begin function
    | x :: q -> if Sys.file_exists x then x else may q
    | [] -> error ()
    end
  in
  begin try may l with
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
