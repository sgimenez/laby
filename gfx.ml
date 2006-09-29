let print = F.print ~l:"gfx"

exception Error

let _ = GtkMain.Main.init ()

let void_p =
  GDraw.pixmap_from_xpm ~transparent:(`WHITE) ~file:"tiles/void.xpm" ()
let exit_p =
  GDraw.pixmap_from_xpm ~transparent:(`WHITE) ~file:"tiles/exit.xpm" ()
let wall_p =
  GDraw.pixmap_from_xpm ~transparent:(`WHITE) ~file:"tiles/wall.xpm" ()
let ant_n_p =
  GDraw.pixmap_from_xpm ~transparent:(`WHITE) ~file:"tiles/ant-n.xpm" ()
let ant_e_p =
  GDraw.pixmap_from_xpm ~transparent:(`WHITE) ~file:"tiles/ant-e.xpm" ()
let ant_s_p =
  GDraw.pixmap_from_xpm ~transparent:(`WHITE) ~file:"tiles/ant-s.xpm" ()
let ant_w_p =
  GDraw.pixmap_from_xpm ~transparent:(`WHITE) ~file:"tiles/ant-w.xpm" ()

let draw_state state (pixmap : GDraw.pixmap) =
  let tile i j (p : GDraw.pixmap) =
    pixmap#put_pixmap ~x:(i*50) ~y:(j*50) p#pixmap
  in
  let p i j t =
    begin match t with
    | `Void -> tile i j void_p
    | `Wall -> tile i j wall_p
    | `Exit -> tile i j exit_p
    end
  in
  Array.iteri (fun j a -> Array.iteri (fun i t -> p i j t) a) state.State.map;
  begin match state.State.pos with
  | i, j, `N -> tile i j ant_n_p
  | i, j, `E -> tile i j ant_e_p
  | i, j, `S -> tile i j ant_s_p
  | i, j, `W -> tile i j ant_w_p
  end

let display_gtk story =
  let state i = List.nth story (List.length story - 1 - i) in
  let posm = List.length story - 1 in
  assert (posm >= 0);
  let pos = ref 0 in
  let bg = ref `WHITE in
  begin try
(*       let _ = GtkMain.Main.init () in *)
      let window = GWindow.window () in
      let delete ev = false in
      let destroy () = GMain.Main.quit () in
      ignore (window#event#connect#delete ~callback:delete);
      ignore (window#connect#destroy ~callback:destroy);
      let vbox = GPack.vbox ~packing:window#add () in
      let width, height = 800, 600 in
      let pixmap = GDraw.pixmap ~width ~height () in
      let px = GMisc.pixmap pixmap ~packing:vbox#add () in
      let hbox = GPack.hbox ~packing:vbox#add ~homogeneous:true () in
      let button label stock =
	GButton.button ~packing:hbox#add ~stock (* ~label *) ()
      in
      let button_first = button "<<" `GOTO_FIRST in
      let button_prev = button "<" `GO_BACK in
      let button_next = button ">" `GO_FORWARD in
      let button_last = button ">>" `GOTO_LAST in
      let update () =
	pixmap#set_foreground !bg;
	let width, height = pixmap#size in
	pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
	draw_state (state !pos) pixmap;
	px#set_pixmap pixmap
      in
      let first () = if !pos > 0 then (pos := 0; update ()) in
      let prev () =  if !pos > 0 then (decr pos; update ()) in
      let next () =  if !pos < posm then (incr pos; update ()) in
      let last () = if !pos < posm then (pos := posm; update ()) in
      ignore (button_first#connect#clicked ~callback:first);
      ignore (button_prev#connect#clicked ~callback:prev);
      ignore (button_next#connect#clicked ~callback:next);
      ignore (button_last#connect#clicked ~callback:last);
      window#show ();
      bg := `COLOR (px#misc#style#light `NORMAL);
      update ();
      ignore (GMain.Main.main ())
    with
    | Gtk.Error m ->
	print ~e:1 (fun () ->
	  F.text "gtk error: <error>" ["error", F.sq m]
	);
	raise Error
  end

