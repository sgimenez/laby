let print = F.print ~l:"version"

let section name entries =
  F.l name (F.v (List.map F.f entries))

let version () =
  print_string (Config.version_string ^ "\n")

let id () =
  F.print (
    section "version"
      [[F.s "string       "; F.s Config.version_string];
       [F.s "base         "; F.s Config.version_base];
       [F.s "current      "; F.s Config.version_current]]
  )

let status () =
  F.print (F.s Config.version_status)

let protocols = ref []

let register_protocol protocol version =
  protocols := [F.sb ~padding:13 protocol; F.sb version] :: !protocols

let protocols () =
  F.print (
    section "protocols" !protocols
  )

let build () =
  F.print (
    section "build"
      [[F.s "ocaml        "; F.sb Config.build_ocaml];
       [F.s "ocaml-lablgtk"; F.sb Config.build_lablgtk]]
  )

let opt =
  let handle () = Opt.Excl (fun () -> version ()) in
  let handle_s =
    begin function
    | "f" | "full" ->
	Opt.Excl (fun () -> id (); protocols (); build ())
    | "i" | "id" -> Opt.Excl (fun () -> id ())
    | "p" | "protocols" -> Opt.Excl (fun () -> protocols ())
    | "b" | "build" -> Opt.Excl (fun () -> build ())
    | "s" | "status" -> Opt.Excl (fun () -> status ())
    | _ -> raise Opt.Invalid
    end
  in
  'v', "version", Some handle, Some handle_s,
  F.x "shows versioning information" []
