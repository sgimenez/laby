let print = F.print ~l:"gfx"

exception Error

type ressources =
    {
      void_p : GdkPixbuf.pixbuf;
      exit_p : GdkPixbuf.pixbuf;
      wall_p : GdkPixbuf.pixbuf;
      rock_p : GdkPixbuf.pixbuf;
      web_p : GdkPixbuf.pixbuf;
      nrock_p : GdkPixbuf.pixbuf;
      nweb_p : GdkPixbuf.pixbuf;
      ant_n_p : GdkPixbuf.pixbuf;
      ant_e_p : GdkPixbuf.pixbuf;
      ant_s_p : GdkPixbuf.pixbuf;
      ant_w_p : GdkPixbuf.pixbuf;
    }

let gtk_init () =
  let _ = GtkMain.Main.init () in
  let pix p = GdkPixbuf.from_file_at_size (Config.conf_path ^ p) 50 50 in
  {
    void_p = pix "tiles/void.svg";
    exit_p = pix "tiles/exit.svg";
    wall_p = pix "tiles/wall.svg";
    rock_p = pix "tiles/rock.svg";
    web_p = pix "tiles/web.svg";
    nrock_p = pix "tiles/nrock.svg";
    nweb_p = pix "tiles/nweb.svg";
    ant_n_p = pix "tiles/ant-n.svg";
    ant_e_p = pix "tiles/ant-e.svg";
    ant_s_p = pix "tiles/ant-s.svg";
    ant_w_p = pix "tiles/ant-w.svg";
  }

let draw_state state ressources (pixmap : GDraw.pixmap) =
  let tile i j p =
    pixmap#put_pixbuf ~x:(25+i*50) ~y:(25+j*50) p
  in
  let i0, j0 = state.State.pos in
  let p i j t =
    begin match t with
    | `Void -> tile i j ressources.void_p
    | `Exit -> if i <> i0 || j <> j0 then tile i j ressources.exit_p
    | `Wall -> tile i j ressources.wall_p
    | `Rock -> tile i j ressources.rock_p
    | `Web -> tile i j ressources.web_p
    | `NRock -> tile i j ressources.nrock_p
    | `NWeb -> tile i j ressources.nweb_p
    end
  in
  Array.iteri (fun j a -> Array.iteri (fun i t -> p i j t) a) state.State.map;
  begin match state.State.dir with
  | `N -> tile i0 j0 ressources.ant_n_p
  | `E -> tile i0 j0 ressources.ant_e_p
  | `S -> tile i0 j0 ressources.ant_s_p
  | `W -> tile i0 j0 ressources.ant_w_p
  end;
  begin match state.State.carry with
  | `Rock -> tile i0 j0 ressources.rock_p
  | _ -> ()
  end

let display_gtk file launch =
  let story = ref [] in
  let pos = ref 0 in
  let next = ref (fun s -> None) in
  let close = ref (fun () -> ()) in
  let init_state = if file = "" then State.basic else State.load file in
  let sizex = Array.length init_state.State.map.(0) in
  let sizey = Array.length init_state.State.map in
  let restart () =
    !close ();
    Random.self_init ();
    story := [init_state];
    pos := 0;
    let i, o, c = launch () in
    close := c;
    next := State.run (i, o);
  in
  restart ();
  let last_state () = List.length !story - 1 in
  let add_state () =
    assert (!story <> []);
    begin match !next (List.hd !story) with
    | None -> false
    | Some state -> story := state :: !story; true
    end
  in
  let state i = List.nth !story (last_state () - i) in
  let bg = ref `WHITE in
  begin try
      let ressources = gtk_init () in
      let window = GWindow.window ~resizable:true () in
      let destroy () =
	window#destroy ();
	GMain.Main.quit () in
      ignore (window#event#connect#delete ~callback:(fun _ -> exit 0));
      ignore (window#connect#destroy ~callback:destroy);
      let width, height = 50 + 50 * sizex, 50 + 50 * sizey in
      let vbox = GPack.vbox ~packing:window#add () in
      let sw = GBin.scrolled_window
	~width:(width+10) ~height:(height+10) ~packing:vbox#add
	~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
      let px = GMisc.image ~packing:sw#add_with_viewport () in
      let pixmap = GDraw.pixmap ~width ~height () in
      let hbox = GPack.hbox ~packing:vbox#pack ~homogeneous:true () in
      let button label stock =
	GButton.button ~packing:hbox#add ~stock (* ~label *) ()
      in
      let tbutton label stock =
	GButton.toggle_button ~packing:hbox#add ~stock (* ~label *) ()
      in
      let button_first = button "|<" `GOTO_FIRST in
      let button_prev = button "<" `GO_BACK in
      let button_next = button ">" `GO_FORWARD in
      let button_last = button ">|" `GOTO_LAST in
      let button_play = tbutton ">>" `MEDIA_PLAY in
      let button_refresh = button "!" `REFRESH in
      let update sound =
	pixmap#set_foreground !bg;
	let width, height = pixmap#size in
	pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
	draw_state (state !pos) ressources pixmap;
	px#set_pixmap pixmap;
	if sound then
	  begin match (state !pos).State.sound with
	  | None -> ()
	  | Some s -> Sound.play s
	  end
      in
      let first () =
	if !pos > 0 then (pos := 0; update false)
      in
      let prev () =
	if !pos > 0 then (decr pos; update false)
      in
      let next () =
	if !pos < last_state () || (!pos = last_state () && add_state ())
	then (incr pos; update true)
	else (button_play#set_active false)
      in
      let play =
	let rid = ref None in
	begin fun () ->
	  begin match !rid with
	  | None ->
	      let callback () = next (); true in
	      rid := Some (GMain.Timeout.add ~ms:500 ~callback);
	  | Some id ->
	      GMain.Timeout.remove id; rid := None
	  end
	end
      in
      let last () =
	if !pos < last_state () then (pos := last_state (); update false)
      in
      let refresh () =
	button_play#set_active false; restart (); update true
      in
      ignore (button_first#connect#clicked ~callback:first);
      ignore (button_prev#connect#clicked ~callback:prev);
      ignore (button_next#connect#clicked ~callback:next);
      ignore (button_last#connect#clicked ~callback:last);
      ignore (button_play#connect#toggled ~callback:play);
      ignore (button_refresh#connect#clicked ~callback:refresh);
      window#set_default_size 640 480;
      window#show ();
      bg := `COLOR (px#misc#style#light `NORMAL);
      update true;
      ignore (GMain.Main.main ())
    with
    | Gtk.Error m ->
	print ~e:1 (
	  F.x "gtk error: <error>" ["error", F.sq m]
	);
	raise Error
  end

