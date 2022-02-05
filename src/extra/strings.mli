module Parser = PamlStrings.Parser

module Lang : sig
  val line_comment : string -> unit Parser.t -> unit Parser.t
  val block_comment : string -> string -> unit Parser.t
  val nested_block_comment : string -> string -> unit Parser.t

  module Ocaml : sig
    val keywords : string list
    val whitespace : unit Parser.t
    val identifier : string Parser.t
    val operator : string Parser.t
    val integer : int Parser.t
    val float : float Parser.t
  end
end
