(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

type tag = string -> string

type d =
    | N
    | T of tag * t
    | S of string
    | L of string * t
    | H of t * t list
    | V of t * t list
    | Q of t
    | P of int * (t -> t) * t
    | X of string * (string * t) list
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Exn of exn
    | Time of float
and t =
    unit -> d

let z m = fun () -> m () ()

let t tag m =
  fun () -> T (tag, m)

let n =
  fun () -> N

let s string =
  fun () -> S string

let h ?(sep=(s " ")) tl =
  fun () -> H (sep, tl)

let v ?(head=(s "  ")) tl =
  fun () -> V (head, tl)

let p i ?(wrap=(fun x -> h [s "("; x ; s ")"])) t =
  fun () -> P (i, wrap, t)

let q m =
  fun () -> Q m

let l s t =
  fun () -> L (s, t)

let x s stl =
  fun () -> X (s, stl)

let int i =
  fun () -> Int i

let float f =
  fun () -> Float f

let string s =
  fun () -> String s

let bool b =
  fun () -> Bool b

let exn e =
  fun () -> Exn e

let time f =
  fun () -> Time f

let use x =
  begin match x () with
    | N -> `N
    | T (s, t) -> `T (s, t)
    | S s -> `S s
    | L (s, t) -> `L (s, t)
    | H (s, l) -> `H (s, l)
    | V (s, l) -> `V (s, l)
    | Q t -> `Q t
    | P (i, f, t) -> `P (i, f, t)
    | X (s, stl) -> `X (s, stl)
    | Int i -> `Int i
    | Float f -> `Float f
    | String s -> `String s
    | Bool b -> `Bool b
    | Exn e -> `Exn e
    | Time f -> `Time f
  end

