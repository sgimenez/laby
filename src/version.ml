let print name entries =
  F.l name (F.v (List.map F.h entries))

let version =
  F.s Config.version_string

let id =
  print "version" [
    [F.s "string       "; F.s Config.version_string];
    [F.s "base         "; F.s Config.version_base];
    [F.s "current      "; F.s Config.version_current];
  ]

let status =
  F.s Config.version_status

let protocols = ref []

let register_protocol protocol version =
  protocols := [F.string protocol; F.s version] :: !protocols

let protocols () =
  print "protocols" !protocols

let build =
  print "build" [
    [F.s "ocaml        "; F.s Config.build_ocaml];
    [F.s "ocaml-lablgtk"; F.s Config.build_lablgtk];
  ]

let opt =
  let noarg () = Opt.Excl (fun () -> version) in
  let arg =
    begin function
    | "f" | "full" ->
	Opt.Excl (
	  fun () -> F.v ~head:F.n [id ; protocols (); build]
	)
    | "i" | "id" -> Opt.Excl (fun () -> id)
    | "p" | "protocols" -> Opt.Excl (fun () -> protocols ())
    | "b" | "build" -> Opt.Excl (fun () -> build)
    | "s" | "status" -> Opt.Excl (fun () -> status)
    | _ ->
	Opt.Invalid (
	  F.x "should be one of: <values>" [
	    "values", F.h ~sep:(F.s ", ") [
	      F.s "full"; F.s "id"; F.s "protocols"; F.s "build"; F.s "status";
	    ];
	  ]
	)
    end
  in
  Opt.make ~short:'v' ~long:"version" ~noarg ~arg
    (F.x "shows versioning information" [])
