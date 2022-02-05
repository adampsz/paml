(** Error types and functions exported from {!PamlCore.Error}. *)
module Error = PamlCore.Error

(** Module signatures from {!PamlCore.T} as {!PamlExtra.T}. *)
module T = struct
  include PamlCore.T
  include PamlExtra.T
end

(** All functors from {!PamlCore.Make} and {!PamlExtra.Make}. *)
module Make = struct
  include PamlCore.Make
  include PamlExtra.Make
end

(** String parsers exported from {!PamlStrings} and {!PamlExtra.Strings}. *)
module Strings = struct
  include PamlStrings
  include PamlExtra.Strings
end
