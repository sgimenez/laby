let print = F.print ~l:"gfx"

exception Error

type ressources =
    {
      void_p : GdkPixbuf.pixbuf;
      exit_p : GdkPixbuf.pixbuf;
      wall_p : GdkPixbuf.pixbuf;
      rock_p : GdkPixbuf.pixbuf;
      ant_n_p : GdkPixbuf.pixbuf;
      ant_e_p : GdkPixbuf.pixbuf;
      ant_s_p : GdkPixbuf.pixbuf;
      ant_w_p : GdkPixbuf.pixbuf;
    }

let gtk_init () =
  let _ = GtkMain.Main.init () in
  {
    void_p = GdkPixbuf.from_file "tiles/void.png";
    exit_p = GdkPixbuf.from_file "tiles/exit.png";
    wall_p = GdkPixbuf.from_file "tiles/wall.png";
    rock_p = GdkPixbuf.from_file "tiles/rock.png";
    ant_n_p = GdkPixbuf.from_file "tiles/ant-n.png";
    ant_e_p = GdkPixbuf.from_file "tiles/ant-e.png";
    ant_s_p = GdkPixbuf.from_file "tiles/ant-s.png";
    ant_w_p = GdkPixbuf.from_file "tiles/ant-w.png";
  }

let draw_state state ressources (pixmap : GDraw.pixmap) =
  let tile i j p =
    pixmap#put_pixbuf ~x:(i*50) ~y:(j*50) p
  in
  let p i j t =
    begin match t with
    | `Void -> tile i j ressources.void_p
    | `Exit -> tile i j ressources.exit_p
    | `Wall -> tile i j ressources.wall_p
    | `Rock -> tile i j ressources.rock_p
    end
  in
  Array.iteri (fun j a -> Array.iteri (fun i t -> p i j t) a) state.State.map;
  let i, j = state.State.pos in
  begin match state.State.dir with
  | `N -> tile i j ressources.ant_n_p
  | `E -> tile i j ressources.ant_e_p
  | `S -> tile i j ressources.ant_s_p
  | `W -> tile i j ressources.ant_w_p
  end;
  begin match state.State.carry with
  | `Rock -> tile i j ressources.rock_p
  | _ -> ()
  end

let display_gtk next =
  let story = ref [State.basic] in
  let last_state () = List.length !story - 1 in
  let add_state () =
    assert (!story <> []);
    begin match next (List.hd !story) with
    | None -> false
    | Some state -> story := state :: !story; true
    end
  in
  let state i = List.nth !story (last_state () - i) in
  let pos = ref 0 in
  let bg = ref `WHITE in
  begin try
      let ressources = gtk_init () in
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
	draw_state (state !pos) ressources pixmap;
	px#set_pixmap pixmap
      in
      let first () =
	if !pos > 0 then (pos := 0; update ())
      in
      let prev () =
	if !pos > 0 then (decr pos; update ())
      in
      let next () =
	if !pos < last_state () || (!pos = last_state () && add_state ())
	then (incr pos; update ())
      in
      let last () =
	if !pos < last_state () then (pos := last_state (); update ())
      in
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

