type query = string * (string -> unit)

type t =
    <
      set_name: string -> unit;
      get_name: string;
      errto: (string -> unit) -> unit;
      set_buf: string -> unit;
      get_buf: string;
      start: unit;
      probe: query option;
      close: unit;
    >

val make : unit -> t

