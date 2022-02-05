let rec factf x = if x <= 1. then 1.0 else x *. factf (x -. 1.)
let modf a b = mod_float (mod_float a b +. b) b

module Parser = struct
  open Paml.Strings.Parser
  open Paml.Strings.Utils
  open Paml.Strings.Ops
  module Pratt = Paml.Make.Pratt (Paml.Strings.Parser)
  module Ocaml = Paml.Strings.Lang.Ocaml

  let rec term () = choice [ !:parens; Ocaml.integer <&> float_of_int; Ocaml.float ]

  and expr () =
    Pratt.easy !:term
    @@ [ [ InfixL (char '+' $> ( +. )); InfixL (char '-' $> ( -. )) ];
         [ InfixL (char '*' $> ( *. ));
           InfixL (char '/' $> ( /. ));
           InfixL (char '%' $> modf)
         ];
         [ PrefixL (char '+' $> ( ~+. )); PrefixL (char '-' $> ( ~-. )) ];
         [ InfixR (char '^' $> ( ** )) ];
         [ SuffixR (char '!' $> factf) ]
       ]

  and parens () = between (char '(') (char ')') !:expr

  let parse = run_string_t (!:expr <* (eof <?> "end of input"))
end

let _ =
  while true do
    Printf.printf "\x1B[90m>\x1B[39m %!";
    match read_line () |> String.trim |> Parser.parse with
    | x -> Format.printf "\x1B[90m=\x1B[39m %g\n%!" x
    | exception End_of_file -> exit 0
    | exception Paml.Error.ParseError [ e ] ->
      Format.eprintf "\n%a\n\n%!" Paml.Error.pp_pretty e
  done
;;
