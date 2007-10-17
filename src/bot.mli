type query = string * (string -> unit)

type t =
    <
      set: string -> unit;
      errto: (string -> unit) -> unit;
      skel: string;
      lang_file: string;
      start: string -> unit;
      probe: query option;
      close: unit;
    >

val make : unit -> t

