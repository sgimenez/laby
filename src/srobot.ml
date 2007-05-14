open Camlp4.PreCast;
open Format;

value _loc = Loc.ghost;

value pl f l = pp_print_string f (String.concat "; " l);

value err loc fmt =
  let () = eprintf "Erreur a la ligne %d, entre les characteres %d et %d.@."
          (Loc.start_line loc) (Loc.start_off loc - Loc.start_bol loc)
                               (Loc.stop_off loc - Loc.start_bol loc) in
  kfprintf (fun _ -> exit 1) err_formatter fmt;

value actions =
  [ "avance"; "gauche"; "droite"; "prend"; "pose"; "regarde"; "pousse" ];

value cases =
  [ "Vide"; "Mur"; "Roche"; "Sortie" ];

value filter = object (self)
  inherit Ast.map as super;
  method expr = fun
  [ <:expr< $lid:x$ >> when List.mem x actions ->
      <:expr< Robot.$lid:x$ () >>
  | <:expr< $uid:x$ >> when List.mem x cases ->
      <:expr< `$uid:x$ >>
  | <:expr< \= >> | <:expr< () >> as x -> x
  | <:expr@loc< $lid:x$ >> ->
      err loc "%S est une mauvaise action, les actions sont [ %a ]@." x pl actions
  | <:expr@loc< $uid:x$ >> ->
      err loc "%S est une mauvaise case, les cases sont [ %a ]@." x pl cases
  | <:expr< while $_$ do { $_$ } >> | <:expr< if $_$ then $_$ else $_$ >> |
    <:expr< do { $_$ } >> | <:expr< $_$ $_$ >> | <:expr< Robot.$_$ >> |
    <:expr< `$_$ >> as x ->
      super#expr x
  | e ->
      let loc = Ast.loc_of_expr e in
      err loc "Construction interdite@." ];
end;

Camlp4.ErrorHandler.register (fun f exn ->
  match exn with
  [ Loc.Exc_located loc e -> err loc "@.@.(la vrai est %a)@." Camlp4.ErrorHandler.print e
  | exn -> raise exn ]);

open Syntax;

EXTEND Gram
  str_item:
  [ [ "tant_que"; e1 = expr; "faire" ; e2 = expr; "fin" ->
        <:str_item< while $e1$ do { $e2$ } >>
    | "si"; e1 = expr; "faire"; "{" ; e2 = expr; "}";
	  "sinon"; "{"; e3 = expr; "}" ->
        <:str_item< if $e1$ then $e2$ else $e3$ >>
    | "si"; e1 = expr; "faire"; "{"; e2 = expr; "}" ->
        <:str_item< if $e1$ then $e2$ else () >>
    ] ];
  expr:
  [ [ "rien_devant" -> <:expr< Robot.regarde () = `Vide >>
    | "mur_devant" -> <:expr< Robot.regarde () = `Mur >>
    | "roche_devant" -> <:expr< Robot.regarde () = `Roche >>
    | "sortie_devant" -> <:expr< Robot.regarde () = `Sortie >>
    ] ];
END;


AstFilters.register_str_item_filter filter#str_item;
