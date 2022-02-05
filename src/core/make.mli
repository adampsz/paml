(** Proper parsing module. *)
module Parser (I : Types.Input) : Types.Parser with module I = I

(** Utils and combinators. *)
module Utils (P : Types.Parser) :
  Types.Utils with type 'a parser = 'a P.t and module I = P.I

(** Operators working on parsers. *)
module Ops (P : Types.Parser) : Types.Ops with type 'a parser = 'a P.t
