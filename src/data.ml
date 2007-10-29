let log = Log.make ["data"]

let rpath_to_string rpath =
  let sep =
    begin match Sys.os_type with
    | "Unix" -> "/"
    | "Cygwin" -> "/"
    | "Win32" -> "\\"
    | _ -> log#error (F.x "unknown os type" []);
	Init.exit 1
    end
  in
  String.concat sep rpath

let check_file f =
  begin try Sys.file_exists f with
  | Sys_error _ ->
      log#warning (
	F.x "cannot access <path>" [
	  "path", F.string f;
	]
      );
      false
  end

let get rpath =
  let ressource = rpath_to_string rpath in
  let raw_dir_dist = [ Config.conf_path; Config.sys_data_path; ] in
  let dir_list = List.filter check_file raw_dir_dist in
  let l = List.map (fun s -> s ^ ressource) dir_list in
  let error () =
    log#error (
      F.x "cannot find ressource <ressource> at: <list>" [
	"ressource", F.string ressource;
	"list", F.v (List.map F.string raw_dir_dist);
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

let get_list rpath =
  let f = Unix.opendir (get rpath) in
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
