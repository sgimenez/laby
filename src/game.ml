
(*
 * Copyright (C) 2007-2014 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

let step lmod msg state =
  begin match lmod#probe msg with
  | None -> None
  | Some (action, reply) ->
      let answer, newstate = State.run action state in
      reply answer;
      Some newstate
  end

let play ~msg ~help ~draw =
  let mktrace lmod level =
    Trace.init (Level.generate level) (step lmod msg)
  in
  let lmod = ref (Mod.dummy "") in
  let level = ref Level.dummy in
  let trace = ref (mktrace !lmod !level) in

  let effects () =
    let action = State.action (Trace.current !trace) in
    let raw f x = f (Fd.render_raw x) in
    Say.action (raw msg) action; Sound.action action
  in

  let draw_update () =
    draw (Trace.current !trace)
  in
  let help_update () =
    begin match Level.help !level with
    | "" -> help None
    | s -> help (Some (!lmod#help s))
    end
  in

object

  method chg_mod s =
    !lmod#stop;
    lmod := s;
    trace := mktrace !lmod !level;
    help_update ();
    !lmod#get_buf

  method chg_level s =
    !lmod#stop;
    level := s;
    trace := mktrace !lmod !level;
    help_update ();
    draw_update ()

  method chg_program s =
    !lmod#set_buf s

  method run =
    !lmod#stop;
    trace := mktrace !lmod !level;
    draw_update ();
    !lmod#start msg

  method next =
    begin match Trace.next !trace with
    | `None -> false
    | `New t -> trace := t; draw_update (); effects (); true
    | `Old t -> trace := t; draw_update (); true
    end

  method prev =
    begin match Trace.prev !trace with
    | `None -> false
    | `Old t -> trace := t; draw_update (); true
    end

  method quit =
    !lmod#stop

end
