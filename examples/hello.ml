open Paml.Strings.Parser
open Paml.Strings.Utils
open Paml.Strings.Ops

(* Basics *)

let hello = string "Hello, "
let name = pattern "[A-Za-z]+"
let exclamation = char '!'

let greeting =
  let* _ = hello
  and* recipient = name
  and* _ = exclamation in
  return recipient
;;

let _bind_greeting =
  bind hello (fun _ ->
      bind name (fun recipient -> bind exclamation (fun _ -> return recipient)))
;;

let _ =
  let a = run_string_t greeting "Hello, paml!" in
  Printf.printf "a: %s\n" a
;;

(* Combinators *)

let dot = char '.'
let good_morning = string "Good morning, "

let another_greeting =
  let* _ = plus hello good_morning
  and* recipients =
    choice
      [ map_with (fun (xs, x) -> xs @ [ x ])
        @@ pair (sep (string ", ") name) (second (string " and ") name);
        map_with (fun x -> [ x ]) name
      ]
  and* _ = plus exclamation dot in
  return recipients
;;

let _ =
  let b = run_string_t another_greeting "Hello, paml!" in
  let c = run_string_t another_greeting "Good morning, ocaml, monads and parsers." in
  List.iter (Printf.printf "b: %s\n") b;
  List.iter (Printf.printf "c: %s\n") c
;;

(* Operators *)

let yet_another_greeting =
  (hello <|> good_morning)
  *> choice
       [ (fun xs x -> xs @ [ x ]) <$> sep (string ", ") name <*> string " and " *> name;
         (fun x -> [ x ]) <$> name
       ]
  <* (exclamation <|> dot)
;;

let _ =
  let d = run_string_t yet_another_greeting "Hello, paml!" in
  let e = run_string_t yet_another_greeting "Good morning, ocaml, monads and parsers." in
  List.iter (Printf.printf "d: %s\n") d;
  List.iter (Printf.printf "e: %s\n") e
;;
