module Input = struct
  type elt = char
  type elts = string
  type t = int * string

  let make str = 0, str

  let next (pos, str) =
    match pos with
    | pos when pos >= String.length str -> None
    | pos -> Some (String.get str pos, (pos + 1, str))
  ;;

  let take n (pos, str) =
    match pos with
    | pos when pos + n > String.length str -> None
    | pos -> Some (String.sub str pos n, (pos + n, str))
  ;;

  let index (pos, _) = pos
  let posl (pos, _) = pos
  let posr (pos, _) = pos
  let span (pos, str) = if pos < String.length str then pos, pos + 1 else pos, pos
  let source (_, str) = str
  let eof (pos, str) = pos >= String.length str
end

module Parser = struct
  include PamlCore.Make.Parser (Input)
  module I = Input

  let source = map input @@ fun i -> Input.source i
  let run_string p i = run p (I.make i)

  let run_string_t p ?file i =
    match run_string p i with
    | Ok x -> x
    | Error e -> raise @@ PamlCore.Error.ParseError [ PamlCore.Error.pretty ?file i e ]
  ;;
end

module Utils = struct
  include PamlCore.Make.Utils (Parser)
  module P = Parser
  module I = Input

  let ( let* ) = P.bind

  (* Funkcje pomocnicze *)
  let msgc c = Format.sprintf "`%s'" (Char.escaped c)
  let msgs s = Format.sprintf "`%s'" (String.escaped s)
  let msgcs s = s |> String.to_seq |> Seq.map msgc |> List.of_seq
  let msgcr a b = Format.sprintf "%s..%s" (msgc a) (msgc b)
  let contains s c = String.index_opt s c |> Option.is_some

  (* Parsery tekstu *)

  let char c = expect (msgc c) @@ sats (( = ) c)
  let char_of s = expects (msgcs s) @@ sats (contains s)
  let char_range a b = expect (msgcr a b) @@ sats (fun c -> a <= c && c <= b)
  let string s = expect (msgs s) @@ verify (( = ) s) @@ P.take @@ String.length s

  let string_of s =
    let lengths = s |> List.map String.length |> List.sort_uniq Int.compare in
    let aux n = verify (Fun.flip List.mem s) @@ P.take n in
    expects (List.map msgs s) @@ choice (List.map aux lengths)
  ;;

  let regexp r =
    let* idx, src = pair index P.source in
    if Str.string_match r src idx
    then P.take @@ String.length @@ Str.matched_string src
    else P.empty
  ;;

  let pattern s = regexp @@ Str.regexp s
  let pattern' s = regexp @@ Str.regexp_case_fold s
  let lf = void (char '\n')
  let cr = void (char '\r')
  let crlf = void (string "\r\n")
  let eol = expect "end of line" @@ choice [ lf; crlf; cr; P.eof ]

  let is_num x = '0' <= x && x <= '9'
  and is_alpha x = ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')

  let num = expect "number" @@ sats is_num
  and alpha = expect "alpha" @@ sats is_alpha

  let alphanum = P.plus alpha num

  let trunc ?(e = "..") l r s =
    let n, m = String.length s, String.length e in
    let re, r = if r >= n then "", n else e, r - m in
    String.sub s l (r - l) ^ re
  ;;

  let trace label =
    let status (i, src) (j, _) = function
      | Ok _ -> Format.sprintf "%S" (String.sub src i (j - i) |> trunc 0 35)
      | Error err -> Format.asprintf "\x1B[91m%a\x1B[39m" PamlCore.Error.pp err
    in
    let enter (i, src) =
      debug_enter label (Format.sprintf "%S" (trunc i (i + 35) src));
      i, src
    and leave j i res =
      debug_leave label (status i j res);
      ()
    in
    P.tap enter leave
  ;;
end

module Ops = struct
  include PamlCore.Make.Ops (Parser)
end
