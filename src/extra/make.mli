module Types = Types
module Strings = Strings

exception IndentationError

module Indent (P : PamlCore.T.Parser) (Ord : Types.OrderedType) :
  Types.Indent with type size = Ord.t and type 'a parser = 'a P.t

module Lexer (S : Types.Space) : Types.Lexer with type 'a parser = 'a S.P.t
module Recovery (P : PamlCore.T.Parser) : Types.Recovery with type 'a parser = 'a P.t
module Pratt (P : PamlCore.T.Parser) : Types.Pratt with type 'a parser = 'a P.t
