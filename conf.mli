(**
   ocaml-dtools
   @author Stéphane Gimenez
*)

(**
   Configuration management module.
*)

type link = string
    (** Type for links between keys *)

type path = link list
    (** Type for paths between keys *)

type ut =
    <
      kind: string option;
      descr: F.t;
      comments: F.t list;
      plug: link -> ut -> unit;
      subs: link list;
      path: path -> ut;
      routes: ut -> path list;
      ut: ut;
    >

(** Type for untyped keys (or keys with unknown type)
    - [kind]: a string describing the type of this key
    - [descr]: a key description/title
    - [comments]: some comments on the key purposes
    - [plug]: a way to plug subkeys
    - [subs]: the list of link names to subkeys
    - [path]: a way to access subkeys
    - [routes]: a way to find paths to an other key
*)

type 'a t =
    <
      kind: string option;
      descr: F.t;
      comments: F.t list;
      plug: link -> ut -> unit;
      subs: link list;
      path: path -> ut;
      routes: ut -> path list;
      ut: ut;
      set_d: 'a option -> unit;
      get_d: 'a option;
      set: 'a -> unit;
      get: 'a;
    >

(** Type for 'a keys
    - [ut]: cast to un untyped key
    - [set_d]: set the default value associated to the key
    - [get_d]: get the default value associated to the key
    - [set]: set the key value according to a user demmand
    - [get]: retrieve the resulting key value
*)

type links = (link * ut) list
    (** A set of connections to others keys *)

exception Undefined of ut
  (** Raised on access to an undefined key (without default value) *)
exception Invalid of string
  (** Raised when an invalid link has been specified *)
exception Unbound of ut * string
  (** Raised when a specified link does not exist *)
exception Bound of ut * string
  (** Raised when a specified link already exist *)
exception Mismatch of ut
  (** Raised on access to a key with a mismatching type *)
exception Cyclic of ut * ut
  (** Raised on cyclic plug *)

exception Wrong_Conf of string * F.t
  (** Raised when bad configuration assignations are encountered  *)
exception File_Wrong_Conf of string * int * F.t
  (** Raised when bad configuration assignations are encountered
      inside configuration files  *)

type 'a builder =
    ?d:'a -> ?p:(ut -> unit) -> ?l:links -> ?comments:F.t list -> F.t -> 'a t
  (** Receipt to build a 'a key *)

val unit : unit builder
val int : int builder
val float : float builder
val bool : bool builder
val string : string builder
val list : string list builder
  (** Some key builders *)

val void :
  ?p:(ut -> unit) -> ?l:links -> ?comments:F.t list -> F.t -> ut
  (** A structural key builder *)

val as_unit : ut -> unit t
val as_int : ut -> int t
val as_float : ut -> float t
val as_bool : ut -> bool t
val as_string : ut -> string t
val as_list : ut -> string list t
  (**
     Casts to specificaly typed keys.
     Raises [Mismatch] on mismatching cast.
  *)

val path_of_string : string -> path
  (** Convert a dot separated string to a path *)
val string_of_path : path -> string
  (** Convert a path to a dot separated string *)

val descr : ?prefix:path -> ut -> F.t list
  (** Generate a description table of a (sub)key *)
val dump :  ?prefix:path -> ut -> string
  (** Dump the confuguration table for a (sub)key *)

val set : ut -> string -> unit -> unit
  (**
     Assign values to configuration keys, according to the given
     correctly formated string: "type key :value"
     Raises [Wrong_Conf] in badly formated cases.
  *)

val load : F.logger -> ?strict: bool -> ut -> string -> unit
  (**
     Read configuration values from the file associated with the given
     filename.
     Raises [File_Wrong_Conf] with filename line and and error message
     in case of a bad configuration file.
  *)

val root : F.logger -> string -> ?l:links -> F.t -> ut
