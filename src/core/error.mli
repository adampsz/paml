(** {1 Errors} *)

(** Error kind. *)
type kind =
  | Expected of string list
  | Message of string
  | Other of exn

(** Error type. Consists of span and error kind. *)
type t = (int * int) * kind

(** Empty error, i.e. error of kind {!Expected} with empty message list. *)
val empty : int * int -> t

(** Merges two errors in a "smart" way. *)
val merge : t -> t -> t

(** Merges two optional errors in a "smart" way. *)
val merge_opt : t option -> t option -> t option

(** {1 Pretty error messages} *)

(** Wrapper around {!t}, additionally containing line-column information and surrounding
    text. Errors of type {!type-pretty} looks like this when pretty-printed:

    {v
  File "<input>", line 1, character 7:
  
     1 | 1 + 2 ]
       |       ^
  
  Error: expected `*', `+', `-', `/' or end of input
    v} *)
type pretty

(** Exception containing list of {!type-pretty}. *)
exception ParseError of pretty list

(** Makes pretty error. *)
val pretty : ?file:string -> string -> t -> pretty

(** Returns original error. *)
val of_pretty : pretty -> t

(** Returns line and column of error start. *)
val pretty_start : pretty -> int * int

(** Returns line and column of error end *)
val pretty_end : pretty -> int * int

(** {1 Pretty printing} *)

(** Prints {!kind}. *)
val pp_kind : Format.formatter -> kind -> unit

(** Prints {!t}. *)
val pp : Format.formatter -> t -> unit

(** Prints {!type-pretty}. *)
val pp_pretty : Format.formatter -> pretty -> unit
