module Parser = PamlStrings.Parser

module Lang = struct
  module P = PamlStrings.Parser
  module U = PamlStrings.Utils

  let ( let* ) = P.bind

  let line_comment left eol =
    let* _ = P.hidden @@ U.string left in
    let* _ = U.until eol P.any in
    P.return ()
  ;;

  let block_comment left right =
    let* _ = P.hidden @@ U.string left in
    let* _ = U.until (U.string right) P.any in
    let* _ = U.message "unclosed block comment" (U.string right) in
    P.return ()
  ;;

  let nested_block_comment left right =
    let rec aux () =
      let* _ = P.hidden @@ U.string left in
      let* _ = U.until (U.string right) (P.plus (P.defer aux) (U.void P.any)) in
      let* _ = U.message "unclosed block comment" (U.string right) in
      P.return ()
    in
    aux ()
  ;;

  module Ocaml = struct
    let keywords =
      "and as asr assert begin class constraint do done downto else end exception \
       external false for fun function functor if in include inherit initializer land \
       lazy let lor lsl lsr lxor match method mod module mutable new nonrec object of \
       open or private rec sig struct then to true try type val virtual when while with \
       != # & && ' ( ) * + , - -. -> . .. .~ : :: := :> ; ;; < <- = > >] >} ? [ [< [> [| \
       ] _ ` { {< | |] || } ~"
      |> String.split_on_char ' '
    ;;

    (* Zbory znaków *)
    let ident_s = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
    and ident_c = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789'"
    and punct_s = "!%&$#+/:<=>?@\\~^|*"
    and punct_c = "-!%&$#+/:<=>?@\\~^|*"

    (* Wyrażenia regularne *)
    let r_space = "[ \010\013\009\026\012]+"
    and r_ident = Format.sprintf "[%s][%s]*" ident_s ident_c
    and r_punct = Format.sprintf "[%s][%s]*" punct_s punct_c

    (* https://ocaml.org/manual/lex.html#integer-literal *)
    and r_int =
      let r_dec = "[0-9][0-9_]*"
      and r_bin = "0[bB[01][01_]*"
      and r_oct = "0[oO][0-7][0-7_]*"
      and r_hex = "0[xX][0-9a-fA-F][0-9a-fA-F_]*" in
      Format.sprintf {|-?\(%s\|%s\|%s\|%s\)|} r_bin r_oct r_hex r_dec

    (* https://ocaml.org/manual/lex.html#float-literal *)
    and r_float =
      let r_dec = "[0-9][0-9_]*"
      and r_dec_f = "\\.[0-9_]*"
      and r_dec_e = "[eE][+\\-]?[0-9][0-9_]*"
      and r_hex = "0[xX][0-9A-Fa-f][0-9A-Fa-f_]*"
      and r_hex_f = "\\.[0-9A-Fa-f_]*"
      and r_hex_e = "[pP][+\\-]?[0-9][0-9_]*" in
      let r_f_dec = Format.sprintf {|-?%s\(%s\)?\(%s\)?|} r_dec r_dec_f r_dec_e
      and r_f_hex = Format.sprintf {|-?%s\(%s\)?\(%s\)?|} r_hex r_hex_f r_hex_e in
      Format.sprintf {|%s\|%s|} r_f_dec r_f_hex
    ;;

    let whitespace = P.hidden @@ U.void @@ U.pattern r_space
    let identifier = U.expect "identifier" @@ U.pattern r_ident
    let operator = U.expect "punctuation" @@ U.pattern r_punct
    let integer = U.expect "integer" @@ U.map_with int_of_string @@ U.pattern r_int
    let float = U.expect "float" @@ U.map_with float_of_string @@ U.pattern r_float
  end
end
