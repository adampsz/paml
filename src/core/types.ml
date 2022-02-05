module type Input = sig
  (** Input data type. *)
  type t

  (** Type of single toked read from input. *)
  type elt

  (** Type of token chunk read from input. *)
  type elts

  (** Returns [Some(x, i)], where [x] is the next token in given input state and [i] is a
      new input state, or [None], when input is empty. *)
  val next : t -> (elt * t) option

  (** Returns [Some(xs, i)], where [xs] is a chunk of next [n] elements from input and [i]
      is a new input state, or [None], when input does not contain [n] tokens. *)
  val take : int -> t -> (elts * t) option

  (** Returns index of the next token in given input state. This function is used mainly
      to determine that parser made progress and input advanced forward. *)
  val index : t -> int

  (** Returns number [l], when [l, r] is a span of the next token in given input state.
      Returned number is used in error messages. *)
  val posl : t -> int

  (** Returns number [r], when [l, r] is a span of the token before next token in given
      input state. *)
  val posr : t -> int

  (** Returns pair [l, r], when [l, r] is a span of the next token in given input state. *)
  val span : t -> int * int

  (** Checks whether there are any tokens left. *)
  val eof : t -> bool
end

module type Parser = sig
  (** {1 Types} *)

  (** Parser. *)
  type 'a t

  (** Input type. *)
  module I : Input

  (** {1 Basic parser operations} *)

  (** [parse p i] runs parsers on given input and returns parsing result. *)
  val run : 'a t -> I.t -> ('a, Error.t) result

  (** [return x] Does not consume any input and returns [x]. Equivalent of Haskell's
      [Monad::parse] or [Applicative::pure]. *)
  val return : 'a -> 'a t

  (** [map p f] applies function [f] to data returned by [p]. Equivalent of Haskell's
      [Functor::fmap]. *)
  val map : 'a t -> ('a -> 'b) -> 'b t

  (** [bind p f] fails when [p] fails, or parsers input with [p] and then continues
      parsing with result of [f]. Equivalent of Haskell's [Monad::(>>=)]. *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [plus p r] tries to parse input using [p], or [r] when it fails. Equivalent of
      Haskell's[MonadPlus::mplus] or [Alternative::(<|>)]. *)
  val plus : 'a t -> 'a t -> 'a t

  (** [empty] never parses anything and always fails. Equivalent of Haskell's
      [MonadPlus::mzero] or [Alternative::empty]. *)
  val empty : 'a t

  (** {1 Input data} *)

  (** Returns current input state. *)
  val input : I.t t

  (** Consumes and returns next token. *)
  val any : I.elt t

  (** Consumes and returns next [n] tokens. *)
  val take : int -> I.elts t

  (** Fails if there are any tokens left on input. *)
  val eof : unit t

  (** {1 Error handling} *)

  (** [throw e] always fails with [e]. *)
  val throw : Error.t -> 'a t

  (** [error e p] fails with error [e] if [p] fails. *)
  val error : Error.t -> 'a t -> 'a t

  (** [label e p] returns result returned by [p], or fails with error of kind [e] and
      guessed span. *)
  val label : Error.kind -> 'a t -> 'a t

  (** [hidden p] silences all errors from [p]. *)
  val hidden : 'a t -> 'a t

  (** [catch p] returns [Ok x], when x is a data returned by [p], or [Error e], when [p]
      fails with error [e]. *)
  val catch : 'a t -> ('a, Error.t) result t

  (** {1 Various helper functions} *)

  (** [peek p] returns result of [p] after restoring parsing state. *)
  val peek : 'a t -> 'a t

  (** [defer f] returns result of parsing with [f ()], but call of [f] is deferred until
      proper parsing takes place. *)
  val defer : (unit -> 'a t) -> 'a t

  (** [state x] returns pair of [get, set] parsers, which could be used to store state.
      State managed by [state] is linear in respect to parser input, i.e. is restored on
      errors and backtracking. *)
  val state : 'a -> 'a t * ('a -> unit t)

  (** {1 Debugging} *)

  (** [tap enter leave p] always calls [enter] when parsing with [p] begins and [leave]
      when it ends, regardless of parsing failures. *)
  val tap : (I.t -> 'x) -> (I.t -> 'x -> ('a, Error.t) result -> unit) -> 'a t -> 'a t
end

module type Utils = sig
  type 'a parser

  module I : Input

  (** {1 Input data} *)

  val index : int parser
  val posl : int parser
  val posr : int parser
  val with_len : 'a parser -> (int * 'a) parser
  val with_elts : 'a parser -> (I.elts * 'a) parser
  val with_span : 'a parser -> ((int * int) * 'a) parser
  val capture : 'a parser -> I.elts parser

  (** {1 Error handling} *)

  val expect : string -> 'a parser -> 'a parser
  val expects : string list -> 'a parser -> 'a parser
  val message : string -> 'a parser -> 'a parser

  (** {1 Utility functions} *)

  val map_with : ('a -> 'b) -> 'a parser -> 'b parser
  val bind_with : ('a -> 'b parser) -> 'a parser -> 'b parser
  val choice : 'a parser list -> 'a parser
  val chain : 'a parser list -> 'a list parser
  val first : 'a parser -> 'b parser -> 'a parser
  val second : 'a parser -> 'b parser -> 'b parser
  val between : 'l parser -> 'r parser -> 'a parser -> 'a parser

  (** {1 Predicates and verification} *)

  val sats : (I.elt -> bool) -> I.elt parser
  val non : 'a parser -> unit parser
  val guard : bool -> unit parser
  val verify : ('a -> bool) -> 'a parser -> 'a parser
  val nonempty : 'a parser -> 'a parser

  (** {1 Built-in types} *)

  val void : 'a parser -> unit parser
  val option : 'a parser -> 'a option parser
  val default : 'a -> 'a parser -> 'a parser
  val either : 'a parser -> 'b parser -> ('a, 'b) Either.t parser
  val pair : 'a parser -> 'b parser -> ('a * 'b) parser

  (** {1 Repetition} *)

  val many : 'a parser -> 'a list parser
  val some : 'a parser -> 'a list parser
  val until : 'b parser -> 'a parser -> 'a list parser
  val count : int -> 'a parser -> 'a list parser
  val sep : 'b parser -> 'a parser -> 'a list parser
  val sept : 'b parser -> 'a parser -> 'a list parser

  (** {1 Debugging} *)

  val debug_enter : string -> string -> unit
  val debug_leave : string -> string -> unit
  val trace : string -> 'a parser -> 'a parser
end

module type Ops = sig
  type 'a parser

  val ( let* ) : 'a parser -> ('a -> 'b parser) -> 'b parser
  val ( and* ) : 'a parser -> 'b parser -> ('a * 'b) parser
  val ( <&> ) : 'a parser -> ('a -> 'b) -> 'b parser
  val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser
  val ( <|> ) : 'a parser -> 'a parser -> 'a parser
  val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser
  val ( <$ ) : 'a -> 'b parser -> 'a parser
  val ( $> ) : 'a parser -> 'b -> 'b parser
  val ( <*> ) : ('a -> 'b) parser -> 'a parser -> 'b parser
  val ( <* ) : 'a parser -> 'b parser -> 'a parser
  val ( *> ) : 'a parser -> 'b parser -> 'b parser
  val ( <??> ) : 'a parser -> string list -> 'a parser
  val ( <?> ) : 'a parser -> string -> 'a parser
  val ( !: ) : (unit -> 'a parser) -> 'a parser
  val ( !? ) : 'a parser -> 'a option parser
  val ( !* ) : 'a parser -> 'a list parser
  val ( !+ ) : 'a parser -> 'a list parser
end
