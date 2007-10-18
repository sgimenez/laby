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
      interprets: GEdit.combo;
      levels: GEdit.combo;
      view_prog: GSourceView.source_view;
      view_mesg: GText.view;
    }

let gtk_init () =
  let _ = GtkMain.Main.init () in
  let pix p =
    let file = Data.get ["tiles"; p ^ ".svg"] in
    GdkPixbuf.from_file_at_size file 50 50
  in
  {
    void_p = pix "void";
    exit_p = pix "exit";
    wall_p = pix "wall";
    rock_p = pix "rock";
    web_p = pix "web";
    nrock_p = pix "nrock";
    nweb_p = pix "nweb";
    ant_n_p = pix "ant-n";
    ant_e_p = pix "ant-e";
    ant_s_p = pix "ant-s";
    ant_w_p = pix "ant-w";
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

let interprets_list =
  List.sort (compare) (Data.get_list ["run"])
let levels_list =
  "demo" :: List.sort (compare) (Data.get_list ["levels"])

let layout () =
  let window = GWindow.window ~resizable:true () in
  let hpaned = GPack.paned `HORIZONTAL ~packing:window#add () in
  hpaned#set_position 500;
  let lvbox = GPack.vbox ~packing:hpaned#add1 () in
  let vpaned = GPack.paned `VERTICAL ~packing:hpaned#add () in
  vpaned#set_position 450;
  let scrolled ?(vpolicy=`ALWAYS) packing =
    GBin.scrolled_window ~packing ~hpolicy:`AUTOMATIC ~vpolicy ()
  in
  let sw_prog = scrolled vpaned#add1 in
  let view_prog =
    GSourceView.source_view
      ~show_line_numbers:true
      ~packing:sw_prog#add ()
  in
  let rvbox = GPack.vbox ~packing:vpaned#add2 () in
  let interprets =
    GEdit.combo
      ~popdown_strings:interprets_list
      ~packing:(rvbox#pack) ()
  in
  let sw_mesg = scrolled rvbox#add in
  let view_mesg = GText.view ~editable:false ~packing:sw_mesg#add  () in
  let levels =
    GEdit.combo
      ~popdown_strings:levels_list
      ~packing:(lvbox#pack) ()
  in
  let sw_laby = scrolled ~vpolicy:`AUTOMATIC lvbox#add in
  let px = GMisc.image ~packing:sw_laby#add_with_viewport () in
  let toolbar = GButton.toolbar ~packing:lvbox#pack ~style:`ICONS () in
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
    interprets = interprets;
    levels = levels;
    view_prog = view_prog;
    view_mesg = view_mesg;
  }

let make_pixmap level =
  let sizex, sizey = Level.size level in
  let width, height = 50 + 50 * sizex, 50 + 50 * sizey in
  GDraw.pixmap ~width ~height ()

let display_gtk () =
  let bot = Bot.make () in
  let level = ref Level.basic in
  let load () = Level.generate !level in
  let b_states = ref [] in
  let c_state = ref (load ()) in
  let n_states = ref [] in
  let bg = ref `WHITE in
  begin try
    let ressources = gtk_init () in
    let c = layout () in
    let pixmap = ref (make_pixmap !level) in
    let destroy () =
      c.window#destroy ();
      GMain.Main.quit ()
    in
    bot#errto (fun s -> c.view_mesg#buffer#insert s);
    let newinterpret () =
      let name = c.interprets#entry#text in
      begin match List.mem name interprets_list with
      | true ->
	  bot#set_buf (c.view_prog#buffer#get_text ());
	  bot#set_name name;
	  c.view_prog#buffer#set_text bot#get_buf;
	  let langf = Data.get ["run"; name; "lang"] in
	  begin match GSourceView.source_language_from_file langf with
	  | None -> log#warning (F.x "cannot load language file" []);
	  | Some l ->
	      c.view_prog#source_buffer#set_language l;
	      c.view_prog#source_buffer#set_highlight true;
	  end
      | false -> ()
      end
    in
    let step state =
      begin match bot#probe with
      | None -> None
      | Some (action, reply) ->
	  let answer, newstate = State.run action state in
	  reply answer;
	  Some newstate
      end
    in
    let bot_stop () =
      bot#close;
      c.view_mesg#buffer#set_text "";
      b_states := []; c_state := load (); n_states := [];
    in
    let bot_start () =
      bot#set_name (c.interprets#entry#text);
      bot#set_buf (c.view_prog#buffer#get_text ());
      bot#start
    in
    let update sound =
      !pixmap#set_foreground !bg;
      let width, height = !pixmap#size in
      !pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
      draw_state !c_state ressources !pixmap;
      c.px#set_pixmap !pixmap;
      if sound then
	begin match !c_state.State.sound with
	| None -> ()
	| Some s -> Sound.play s
	end
    in
    let refresh () =
      c.button_play#set_active false;
      bot_stop ();
      bot_start ();
      update true
    in
    let newlevel () =
      let name = c.levels#entry#text in
      begin match List.mem name levels_list with
       | true ->
	   level :=
	   begin match name with
	   | "demo" -> Level.basic
	   | n -> Level.load (Data.get ["levels"; n])
	   end;
	   pixmap := make_pixmap !level;
	   c.button_play#set_active false;
	   bot_stop ();
	   update true
       | false -> ()
      end
    in
    c.interprets#entry#set_text "ocaml";
    newinterpret ();
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
    ignore (c.window#event#connect#delete ~callback:(fun _ -> exit 0));
    ignore (c.window#connect#destroy ~callback:destroy);
    ignore (c.button_first#connect#clicked ~callback:first);
    ignore (c.button_prev#connect#clicked ~callback:prev);
    ignore (c.button_next#connect#clicked ~callback:next);
    ignore (c.button_last#connect#clicked ~callback:last);
    ignore (c.button_play#connect#toggled ~callback:play);
    ignore (c.button_refresh#connect#clicked ~callback:refresh);
    ignore (c.interprets#entry#connect#changed ~callback:newinterpret);
    ignore (c.levels#entry#connect#changed ~callback:newlevel);
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
