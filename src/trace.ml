
(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

type 'a t =
    {
      prev: 'a list;
      current: 'a;
      next: 'a list;
      step: 'a -> 'a option;
    }

let init obj step =
  {
    prev = [];
    current = obj;
    next = [];
    step = step;
  }

let current t =
  t.current

let next t =
  begin match t.next with
  | [] ->
      begin match t.step t.current with
      | None -> `None
      | Some obj ->
	  `New {
	    prev = t.current :: t.prev;
	    current = obj;
	    next = [];
	    step = t.step;
	  }
      end
  | obj :: next ->
      `Old {
	prev = t.current :: t.prev;
	current = obj;
	next = next;
	step = t.step;
      }
  end


let prev t =
  begin match t.prev with
  | [] -> `None
  | obj :: prev ->
      `Old {
	prev = prev;
	current = obj;
	next = t.current :: t.next;
	step = t.step;
      }
  end



