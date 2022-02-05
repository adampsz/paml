module Ast = struct
  type expr =
    | Func of string * expr list
    | Variable of string
    | Number of int
    | Any
    | Cut

  type clause = Clause of expr * expr list
  type program = Program of clause list

  let pp_block_open f = Format.fprintf f "@[<hv 0>%s(@;<0 2>@[<hv 0>"
  let pp_block_close f = Format.fprintf f "@])@]"

  let rec pp_expr f = function
    | Variable v -> Format.fprintf f "Variable(%s)" v
    | Number v -> Format.fprintf f "Number(%d)" v
    | Any -> Format.fprintf f "Any"
    | Cut -> Format.fprintf f "Cut"
    | Func (v, xs) ->
      Format.fprintf f "%a%s" pp_block_open "Func" v;
      List.iter (Format.fprintf f ",@ %a" pp_expr) xs;
      pp_block_close f
  ;;

  let pp_clause f (Clause (hd, xs)) =
    pp_block_open f "Clause";
    Format.fprintf f "%a" pp_expr hd;
    List.iter (Format.fprintf f ",@ %a" pp_expr) xs;
    pp_block_close f
  ;;

  let pp f (Program xs) =
    pp_block_open f "Program";
    List.iteri
      (fun i x ->
        if i == 0
        then Format.fprintf f "%a" pp_clause x
        else Format.fprintf f ",@ %a" pp_clause x)
      xs;
    pp_block_close f
  ;;
end

module Lexer = struct
  module P = Paml.Strings.Parser
  module U = Paml.Strings.Utils
  module L = Paml.Strings.Lang

  include Paml.Make.Lexer (struct
    module P = Paml.Strings.Parser

    let space = U.void @@ U.pattern "[ \t\n\r]+"
    let line_comment = L.line_comment "//" U.eol
    let block_comment = L.nested_block_comment "/*" "*/"
  end)

  module Ocaml = Paml.Strings.Lang.Ocaml

  let r_symbol = "[a-z_][0-9A-Za-z_]*"
  and r_punctuation = "[-!%*+/:<=>^]+"
  and r_variable = "[A-Z][0-9A-Za-z_]*"
  and reserved = "_ :- !" |> String.split_on_char ' '

  let atom id = lexeme (U.string id)
  let symbol, keyword = identifier reserved (U.pattern r_symbol)
  let punctuation, operator = identifier reserved (U.pattern r_punctuation)
  let variable, _ = identifier reserved (U.pattern r_variable)
  let integer = lexeme @@ Ocaml.integer
end

module Parser = struct
  module P = Paml.Strings.Parser
  module U = Paml.Strings.Utils
  module E = Paml.Make.Pratt (Paml.Strings.Parser)
  open Paml.Strings.Ops

  let rec term () =
    U.choice
      [ !:func;
        !:list;
        Lexer.keyword "_" $> Ast.Any;
        Lexer.operator "!" $> Ast.Cut;
        (Lexer.variable <&> fun v -> Ast.Variable v);
        (Lexer.integer <&> fun i -> Ast.Number i)
      ]

  and expr () =
    let op_il id = E.InfixL (Lexer.operator id $> fun x y -> Ast.Func (id, [ x; y ]))
    and op_ir id = E.InfixR (Lexer.operator id $> fun x y -> Ast.Func (id, [ x; y ]))
    and kw_il id = E.InfixL (Lexer.keyword id $> fun x y -> Ast.Func (id, [ x; y ])) in
    U.expect "expression"
    @@ E.easy
         !:term
         [ [ kw_il "is" ];
           [ op_il "<"; op_il ">"; op_il "="; op_il "!=" ];
           [ op_il "+"; op_il "-" ];
           [ op_il "*"; op_il "/"; op_il "%" ];
           [ op_ir "^" ]
         ]

  and func () =
    let* name = Lexer.symbol <|> Lexer.punctuation in
    let terms = U.sep (Lexer.atom ",") !:expr in
    let* args = U.default [] @@ (Lexer.atom "(" *> terms <* Lexer.atom ")") in
    P.return (Ast.Func (name, args))

  and list () =
    U.expect "list"
    @@ let* _ = Lexer.atom "[" in
       let* xs = U.default [] @@ U.sep (Lexer.atom ",") !:expr in
       let* tl = U.default (Ast.Func ("nil", [])) (Lexer.atom "|" *> !:expr) in
       let* _ = Lexer.atom "]" in
       P.return (List.fold_right (fun x tl -> Ast.Func ("cons", [ x; tl ])) xs tl)
  ;;

  let clause =
    let* head = !:expr in
    let terms = U.sep (Lexer.atom ",") !:expr in
    let* body = U.default [] @@ (Lexer.operator ":-" *> terms) <* Lexer.atom "." in
    P.return (Ast.Clause (head, body))
  ;;

  let program =
    U.map_with (fun xs -> Ast.Program xs) @@ U.many clause <* U.expect "end of file" P.eof
  ;;

  let parse = P.run_string_t program
end

let _ =
  let rec input cont =
    Printf.printf "\x1B[90m%s\x1B[39m %!" (if cont then ".." else ">>");
    let line = read_line () |> String.trim in
    if String.ends_with ~suffix:"." line then line else line ^ "\n" ^ input true
  in
  while true do
    match input false |> String.trim |> Parser.parse with
    | ast -> Format.printf "%a\n%!" Ast.pp ast
    | exception Paml.Error.ParseError [ e ] ->
      Format.eprintf "\n%a\n\n%!" Paml.Error.pp_pretty e
    | exception End_of_file -> exit 0
  done
;;
