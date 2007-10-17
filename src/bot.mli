type query = string * (string -> unit)

type t =
    <
      errto: (string -> unit) -> unit;
      skel: string;
      lang_file: string;
      start: string -> unit;
      probe: query option;
      close: unit;
    >

val load : string -> t

