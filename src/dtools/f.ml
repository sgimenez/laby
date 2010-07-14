(*
   Copyright (C) 2007-2010 Stéphane Gimenez
   You have permission to copy, modify, and redistribute under the
   terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
*)

(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

type t =
    | N
    | T of tag * t
    | S of string
    | L of string * t
    | H of t * t list
    | V of t * t list
    | Q of t
    | P of int * (t -> t) * t
    | X of string * string * (string * t) list
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Exn of string * exn
    | Time of float
    | Lazy of (unit -> t)

and tag = t -> t

let z fn =
  Lazy fn

let t tag m =
  T (tag, m)

let n =
  N

let s string =
  S string

let b tl =
  H (N, tl)

let h ?(sep=(s " ")) tl =
  H (sep, tl)

let v ?(head=n) tl =
  V (head, tl)

let i tl =
  V (s "  ", tl)

let p i ?(wrap=(fun x -> H (N, [s "("; x ; s ")"]))) t =
  P (i, wrap, t)

let q m =
  Q m

let l s t =
  L (s, t)

let x s stl =
  X ("", s, stl)

let xs special s stl =
  X (special, s, stl)

let int i =
  Int i

let float f =
  Float f

let string s =
  String s

let bool b =
  Bool b

let exn ?(bt="") e =
  Exn (bt, e)

let time f =
  Time f

let use x =
  begin match x with
    | N -> `N
    | T (s, t) -> `T (s, t)
    | S s -> `S s
    | L (s, t) -> `L (s, t)
    | H (s, l) -> `H (s, l)
    | V (s, l) -> `V (s, l)
    | Q t -> `Q t
    | P (i, f, t) -> `P (i, f, t)
    | X (special, s, stl) -> `X (special, s, stl)
    | Int i -> `Int i
    | Float f -> `Float f
    | String s -> `String s
    | Bool b -> `Bool b
    | Exn (bt, e) -> `Exn (bt, e)
    | Time f -> `Time f
    | Lazy fn -> `Lazy fn
  end

