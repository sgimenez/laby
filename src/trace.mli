(*
 * Copyright (C) 2007-2009 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)

type 'a t

val init : 'a -> ('a -> 'a option) -> 'a t

val current : 'a t -> 'a
val next : 'a t -> [ `None | `Old of 'a t | `New of 'a t ]
val prev : 'a t -> [ `None | `Old of 'a t ]
