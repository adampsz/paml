module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type Indent = sig
  type 'a parser
  type size

  val indentation : size -> size parser -> size parser
  val indent : unit parser
  val dedent : unit parser
end

module type Space = sig
  module P : PamlCore.T.Parser

  val space : unit P.t
  val line_comment : unit P.t
  val block_comment : unit P.t
end

module type Lexer = sig
  type 'a parser

  val spaces : unit parser
  val lexeme : 'a parser -> 'a parser
  val identifier : string list -> string parser -> string parser * (string -> unit parser)
end

module type Recovery = sig
  type 'a parser

  val get : PamlCore.Error.t list parser
  val report : PamlCore.Error.t -> unit parser
  val rollback_with : (PamlCore.Error.t -> 'a parser) -> 'a parser -> 'a parser
  val rollback : 'a parser -> 'a option parser
  val recover : string -> 'a parser -> 'a option parser
end

module type Pratt = sig
  type 'a parser

  (** Operators used by {!easy}. *)
  type 'a operator =
    | PrefixL of ('a -> 'a) parser
    | SuffixR of ('a -> 'a) parser
    | InfixL of ('a -> 'a -> 'a) parser
    | InfixR of ('a -> 'a -> 'a) parser

  (** [easy term operators] is a operator-precedence parser. Operator table is a list of
      operator groups with the first group having the least binding power. *)
  val easy : 'a parser -> 'a operator list list -> 'a parser

  (** [make term prefix infix suffix] is a operator-precedence parser. [prefix], [infix],
      and [suffix] parsers should return operator binding powers and their constructors
      of, respectively, prefix, infix and suffix operators. *)
  val make
    :  'a parser ->
    (int * ('a -> 'a)) parser ->
    (int * int * ('a -> 'a -> 'a)) parser ->
    (int * ('a -> 'a)) parser ->
    'a parser
end
