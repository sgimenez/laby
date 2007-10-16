type query = string * (string -> unit)

type t =
    <
      errto: (string -> unit) -> unit;
      skel: string;
      start: string -> unit;
      probe: query option;
      close: unit;
    >

val caml : t
