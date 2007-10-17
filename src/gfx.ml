let log = Log.make ["gfx"]

exception Error of F.t

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

type controls =
    {
      window: GWindow.window;
      button_first: GButton.tool_button;
      button_prev: GButton.tool_button;
      button_next: GButton.tool_button;
      button_last: GButton.tool_button;
      button_play: GButton.toggle_tool_button;
      button_refresh: GButton.tool_button;
      px: GMisc.image;
      view_prog: GSourceView.source_view;
      view_mesg: GText.view;
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


let layout () =
  let window = GWindow.window ~resizable:true () in
  let hpaned = GPack.paned `HORIZONTAL ~packing:window#add () in
  hpaned#set_position 500;
  let vbox = GPack.vbox ~packing:hpaned#add1 () in
  let vpaned = GPack.paned `VERTICAL ~packing:hpaned#add () in
  vpaned#set_position 500;
  let scrolled ?(vpolicy=`ALWAYS) packing =
    GBin.scrolled_window ~packing ~hpolicy:`AUTOMATIC ~vpolicy ()
  in
  let sw_prog = scrolled vpaned#add1 in
  let view_prog =
    GSourceView.source_view
      ~show_line_numbers:true
      ~packing:sw_prog#add ()
  in
  let lang_file = Config.conf_path ^ "run/ocaml/lang" in
  begin match GSourceView.source_language_from_file lang_file with
  | None -> log#warning (F.x "cannot load language file" []);
  | Some l ->
      view_prog#source_buffer#set_language l;
      view_prog#source_buffer#set_highlight true;
  end;
  let sw_mesg = scrolled vpaned#add2 in
  let view_mesg = GText.view ~editable:false ~packing:sw_mesg#add  () in
  let sw_laby = scrolled ~vpolicy:`AUTOMATIC vbox#add in
  let px = GMisc.image ~packing:sw_laby#add_with_viewport () in
  let toolbar = GButton.toolbar ~packing:vbox#pack ~style:`ICONS () in
  let button stock =
    GButton.tool_button ~packing:toolbar#insert ~stock ()
  in
  let tbutton stock =
    GButton.toggle_tool_button ~packing:toolbar#insert ~stock ()
  in
  let button_first = button `GOTO_FIRST in
  let button_prev = button `GO_BACK in
  let button_next = button `GO_FORWARD in
  let button_last = button `GOTO_LAST in
  let _ =
    GButton.separator_tool_item
      ~expand:false ~draw:true ~packing:toolbar#insert () in
  let button_play = tbutton `MEDIA_PLAY in
  let _ =
    GButton.separator_tool_item
      ~expand:true ~draw:false ~packing:toolbar#insert () in
  let button_refresh = button `REFRESH in
  {
    window = window;
    button_first = button_first;
    button_prev = button_prev;
    button_next = button_next;
    button_last = button_last;
    button_play = button_play;
    button_refresh = button_refresh;
    px = px;
    view_prog = view_prog;
    view_mesg = view_mesg;
  }

let display_gtk () =
  let bot = Bot.load "ocaml" in
  let level = Level.basic in
  let load () = Level.generate level in
  let b_states = ref [] in
  let c_state = ref (load ()) in
  let n_states = ref [] in
  let sizex = Array.length !c_state.State.map.(0) in
  let sizey = Array.length !c_state.State.map in
  let bg = ref `WHITE in
  begin try
    let ressources = gtk_init () in
    let c = layout () in
    let destroy () =
      c.window#destroy ();
      GMain.Main.quit ()
    in
    let width, height = 50 + 50 * sizex, 50 + 50 * sizey in
    let pixmap = GDraw.pixmap ~width ~height () in
    let buffer = c.view_prog#buffer in
    let print_mesg = c.view_mesg#buffer#insert in
    bot#errto (fun s -> print_mesg s);
    buffer#insert (bot#skel);
    let clear_mesg () = c.view_mesg#buffer#set_text "" in
    let step state =
      begin match bot#probe with
      | None -> None
      | Some (action, reply) ->
	  let answer, newstate = State.run action state in
	  reply answer;
	  Some newstate
      end
    in
    let restart prog =
      bot#close;
      clear_mesg ();
      b_states := []; c_state := load (); n_states := [];
      bot#start prog
    in
    let update sound =
      pixmap#set_foreground !bg;
      let width, height = pixmap#size in
      pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
      draw_state !c_state ressources pixmap;
      c.px#set_pixmap pixmap;
      if sound then
	begin match !c_state.State.sound with
	| None -> ()
	| Some s -> Sound.play s
	end
    in
    let first () =
      if !b_states <> [] then
	begin match List.rev !b_states @ (!c_state :: !n_states) with
	| x :: q ->
	    b_states := []; c_state := x; n_states := q; update false
	| _ -> assert false
	end
    in
    let prev () =
      begin match !b_states with
      | [] -> ()
      | x :: q ->
	  b_states := q; n_states := !c_state :: !n_states; c_state := x;
	  update false
      end
    in
    let next () =
      begin match !n_states with
      | [] ->
	  begin match step !c_state with
	  | Some x ->
	      b_states := !c_state :: !b_states; c_state := x;
	      update true
	  | None -> c.button_play#set_active false
	  end
      | x :: q ->
	  b_states := !c_state :: !b_states; c_state := x; n_states := q;
	  update false
      end
    in
    let last () =
      if !n_states <> [] then
	begin match List.rev !n_states @ (!c_state :: !b_states) with
	| x :: q ->
	    b_states := q; c_state := x; n_states := []; update false
	| _ -> assert false
	end
    in
    let play =
      let rid = ref None in
      begin fun () ->
	begin match !rid with
	| None ->
	    c.button_play#set_stock_id `MEDIA_PAUSE;
	    let callback () = next (); true in
	    rid := Some (GMain.Timeout.add ~ms:500 ~callback);
	| Some id ->
	    c.button_play#set_stock_id `MEDIA_PLAY;
	    GMain.Timeout.remove id; rid := None
	end
      end
    in
    let refresh () =
      c.button_play#set_active false;
      restart (buffer#get_text ());
      update true
    in
    ignore (c.window#event#connect#delete ~callback:(fun _ -> exit 0));
    ignore (c.window#connect#destroy ~callback:destroy);
    ignore (c.button_first#connect#clicked ~callback:first);
    ignore (c.button_prev#connect#clicked ~callback:prev);
    ignore (c.button_next#connect#clicked ~callback:next);
    ignore (c.button_last#connect#clicked ~callback:last);
    ignore (c.button_play#connect#toggled ~callback:play);
    ignore (c.button_refresh#connect#clicked ~callback:refresh);
    c.window#set_default_size 900 600;
    c.window#show ();
    (* bg color has to be retrieved after c.window#show *)
    bg := `COLOR (c.px#misc#style#light `NORMAL);
    update true;
    ignore (GMain.Main.main ())
  with
  | Gtk.Error m ->
      raise (
	Error (
	  F.x "gtk error: <error>" ["error", F.string m]
	)
      )
  end
