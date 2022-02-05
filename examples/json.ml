(* https://www.json.org *)

module Data = struct
  type t =
    | Null
    | Bool of bool
    | Number of float
    | String of string
    | Array of t list
    | Object of (string * t) list

  let rec pp f = function
    | Null -> Format.fprintf f "null"
    | Bool x -> Format.fprintf f "%b" x
    | Number x -> Format.fprintf f "%g" x
    | String x -> Format.fprintf f "%s" x
    | Array xs ->
      Format.fprintf f "@[<v>";
      List.iter (Format.fprintf f "@,- %a" pp) xs;
      Format.fprintf f "@]"
    | Object yxs ->
      Format.fprintf f "@[<v>";
      List.iter (fun (y, x) -> Format.fprintf f "@,%s: %a" y pp x) yxs;
      Format.fprintf f "@]"
  ;;
end

module Parser = struct
  open Paml.Strings.Parser
  open Paml.Strings.Utils
  open Paml.Strings.Ops
  module R = Paml.Make.Recovery (Paml.Strings.Parser)

  let whitespace = hidden @@ pattern "[ \t\r\n]*"
  let lexeme p = p <* whitespace
  let atom c = lexeme @@ char c

  let json_number =
    expect "number"
    @@ map_with float_of_string
    @@ capture
    @@ pattern {|-?\(0\|[1-9][0-9]*\)\(\.[-0-9]+\)?\([eE][\-=]?[0-9]+\)?|}
  ;;

  let json_string =
    expect "string"
    @@ between (char '"') (char '"')
    @@ map_with Scanf.unescaped
    @@ capture
    @@ until (char '"')
    @@ (char '\\' *> char_of {|"\/bfnrt|} <|> any)
  ;;

  let rec json_array () =
    between (atom '[') (R.recover "missing closing bracket" (atom ']'))
    @@ default []
    @@ sep (R.recover "missing comma" (atom ','))
    @@ !:json_value

  and json_object () =
    between (atom '{') (R.recover "missing closing brace" (atom '}'))
    @@ default []
    @@ sep (R.recover "missing comma" (atom ','))
    @@ pair (lexeme json_string <* R.recover "missing colon" (atom ':')) !:json_value

  and json_value () =
    lexeme
    @@ expect "value"
    @@ choice
         [ string "null" $> Data.Null;
           string "true" $> Data.Bool true;
           string "false" $> Data.Bool false;
           (json_number <&> fun x -> Data.Number x);
           (json_string <&> fun x -> Data.String x);
           (!:json_array <&> fun x -> Data.Array x);
           (!:json_object <&> fun x -> Data.Object x)
         ]
  ;;

  let parse =
    run_string_t
    @@ let* x = whitespace *> !:json_value in
       let* _ = R.recover "leftover tokens" (expect "end of file" eof) in
       let* r = R.get in
       return (x, r)
  ;;
end

let _ =
  let rec input () =
    match read_line () with
    | line -> line ^ "\n" ^ input ()
    | exception End_of_file -> ""
  in
  let input = input () in
  let data, reports =
    match Parser.parse input with
    | data, reports -> data, reports
    | exception Paml.Error.ParseError [ e ] ->
      Format.eprintf "\n%a\n" Paml.Error.pp_pretty e;
      exit 1
  in
  Format.printf "%a\n\n" Data.pp data;
  List.iter
    (fun e ->
      let e = Paml.Error.pretty input e in
      Format.eprintf "\n%a\n" Paml.Error.pp_pretty e)
    reports
;;
