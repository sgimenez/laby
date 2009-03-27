(*
   Copyright (C) 2007-2009 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

let log = Log.make ["res"]

type t = string list

let conf =
  Conf.void
    (F.x "resources configuration" [])

let conf_paths =
  Conf.list
    ~p:(conf#plug "paths")
    (F.x "resource directories" [])

exception Error of F.t

let rpath_to_string =
  let sep =
    begin match Sys.os_type with
    | "Unix" -> "/"
    | "Cygwin" -> "/"
    | "Win32" -> "\\"
    | _ -> assert false
    end
  in
  fun rpath -> String.concat sep rpath

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
  let resource = rpath_to_string rpath in
  let raw_dir_dist = conf_paths#get in
  let dir_list = List.filter check_file raw_dir_dist in
  let l = List.map (fun s -> s ^ resource) dir_list in
  let error () =
    raise (Error (
      F.x "cannot find resource <resource> in: <list>" [
	"resource", F.string resource;
	"list", F.v (List.map F.string raw_dir_dist);
      ]
    ));
  in
  let found x =
    log#debug 2 (
      F.x "found resource <resource> at <location>" [
	"resource", F.string resource;
	"location", F.string x;
      ]
    );
  in
  let rec may =
    begin function
    | x :: q ->
	begin try
	  begin match Sys.file_exists x with
	  | true -> found x; x
	  | false -> may q
	  end
	with
	| Sys_error _ -> may q
	end
    | [] -> error ()
    end
  in
  may l

let get_list ?ext rpath =
  let f = Unix.opendir (get rpath) in
  let list = ref [] in
  let add e =
    begin match ext with
    | None -> list := e :: !list
    | Some ext ->
	let l = String.length ext + 1 in
	if String.length e >= l then
	  if String.sub e (String.length e - l) l = "." ^ ext then
	    list := e :: !list
    end
  in
  begin try
    while true do
      begin match Unix.readdir f with
      | "." | ".." -> ()
      | e -> add e
      end
    done;
  with
  | End_of_file -> ()
  end;
  Unix.closedir f;
  !list

let use path f =
  let filename = get path in
  begin try
    let file = open_in_bin filename in
    f filename file;
    close_in file;
  with
  | Sys_error m ->
      log#warning (
	F.x "failed to open resource: <resource>"
	  ["resource", F.string (rpath_to_string path)]
      );
  end
