
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let section name entries =
  F.l name (F.i (List.map F.h entries))

let version () =
  print_string (Config.version_string ^ "\n");
  exit 0

let id () =
  section "id"
    [[F.s "string       "; F.s Config.version_string];
     [F.s "base         "; F.s Config.version_base];
     [F.s "current      "; F.s Config.version_current]]

let status () =
  F.s Config.version_status

let protocols = ref []

let register_protocol protocol version =
  protocols := [F.string protocol; F.s version] :: !protocols

let protocols () =
  section "protocols" !protocols

let build () =
  section "build" [
    [F.s "ocaml                  "; F.s Config.build_ocaml];
    [F.s "ocaml-lablgtk          "; F.s Config.build_lablgtk];
    [F.s "ocaml-lablgtksourceview"; F.s Config.build_lablgtksourceview];
  ]

let disp l () =
  F.l "versions" (F.i (List.map (fun f -> f ()) l))

let opt =
  let action =
    begin function
    | None -> Opt.Excl (fun () -> version ())
    | Some s ->
	begin match s with
	| "f" | "full" -> Opt.Excl (disp [id; protocols; build])
	| "i" | "id" -> Opt.Excl id
	| "p" | "protocols" -> Opt.Excl protocols
	| "b" | "build" -> Opt.Excl build
	| "s" | "status" -> Opt.Excl status
	| _ ->
	    let argl =
	      [ "full"; "id"; "protocols"; "build"; "status" ]
	    in
 	    Opt.Invalid (
	      F.x "version allows arguments: <arguments>" [
		"arguments", F.h ~sep:(F.s ", ") (List.map F.s argl)
	      ]
	    )
	end
    end
  in
  Opt.make ~short:'v' ~long:"version" (`Any action)
    (F.x "show versioning information" [])
