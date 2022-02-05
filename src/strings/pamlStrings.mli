module Input : sig
  include PamlCore.T.Input with type elt = char and type elts = string

  val make : string -> t
  val take : int -> t -> (string * t) option
  val source : t -> string
end

module Parser : sig
  include PamlCore.T.Parser with module I = Input

  val source : string t
  val run_string : 'a t -> string -> ('a, PamlCore.Error.t) result
  val run_string_t : 'a t -> ?file:string -> string -> 'a
end

module Utils : sig
  include PamlCore.T.Utils with type 'a parser = 'a Parser.t and module I = Input

  (** Basic string parsers *)

  val char : char -> char parser
  val char_of : string -> char parser
  val char_range : char -> char -> char parser
  val string : string -> string parser
  val string_of : string list -> string parser
  val regexp : Str.regexp -> string parser
  val pattern : string -> string parser
  val pattern' : string -> string parser

  (** Commonly used parsers *)

  val lf : unit parser
  val cr : unit parser
  val crlf : unit parser
  val eol : unit parser
  val num : char parser
  val alpha : char parser
  val alphanum : char parser
end

module Ops : sig
  include PamlCore.T.Ops with type 'a parser = 'a Parser.t
end
