module Types = Types
module Strings = Strings

exception IndentationError

module Indent (P : PamlCore.T.Parser) (Ord : Types.OrderedType) = struct
  type size = Ord.t
  type 'a parser = 'a P.t

  module U = PamlCore.Make.Utils (P)

  let ( let* ) = P.bind
  let get, set = P.state (0, [])

  let compare x y =
    try Some (Ord.compare x y) with
    | IndentationError -> None
  ;;

  let indent =
    let* q, st = get in
    set (q + 1, st)
  ;;

  let dedent =
    P.bind get
    @@ function
    | 0, [] -> P.empty
    | 0, _ :: st -> set (0, st)
    | q, st -> set (max 0 (q - 1), st)
  ;;

  let hd x = function
    | [] -> x
    | x :: _ -> x
  ;;

  let throw_msg sp msg = P.throw (sp, PamlCore.Error.Message msg)

  let indent_exact sp x y =
    match compare x y with
    | None -> throw_msg sp "incorrect indentation"
    | Some r when r > 0 -> throw_msg sp "indentation too big"
    | Some r when r < 0 -> throw_msg sp "indentation too small"
    | Some _ -> P.return x
  ;;

  let indent_more sp x y =
    match compare x y with
    | None -> throw_msg sp "incorrect indentation"
    | Some r when r <= 0 -> throw_msg sp "indentation too small"
    | Some _ -> P.return x
  ;;

  let indentation zero p =
    let* q, st = get in
    let* sp, x = U.with_span p in
    if q = 0
    then indent_exact sp x (hd zero st)
    else
      let* y = indent_more sp x (hd zero st) in
      P.map (set (max 0 (q - 1), y :: st)) (Fun.const y)
  ;;
end

module Lexer (S : Types.Space) = struct
  module P = S.P

  type 'a parser = 'a P.t

  module U = PamlCore.Make.Utils (P)
  module SSet = Set.Make (String)

  let spaces =
    U.void @@ U.many @@ U.choice [ P.hidden S.space; S.line_comment; S.block_comment ]
  ;;

  let lexeme p = U.first p spaces

  let identifier res p =
    let res = SSet.of_list res in
    let is_reserved = Fun.flip SSet.mem res in
    let ident = lexeme @@ U.verify (Fun.negate is_reserved) @@ p
    and keyword id = lexeme @@ U.void @@ U.verify (( = ) id) @@ p in
    ident, keyword
  ;;
end

module Recovery (P : PamlCore.T.Parser) = struct
  type 'a parser = 'a P.t

  let ( let* ) = P.bind
  let rget, rset = P.state []

  let get =
    let* xs = rget in
    P.return (List.sort (fun a b -> compare (fst a) (fst b)) xs)
  ;;

  let report err =
    let* xs = rget in
    rset (err :: xs)
  ;;

  let rollback_with f p =
    P.bind (P.catch p)
    @@ function
    | Ok x -> P.return x
    | Error e -> f e
  ;;

  let rollback p =
    P.bind (P.catch p)
    @@ function
    | Ok x -> P.return (Some x)
    | Error e -> P.map (report e) (Fun.const None)
  ;;

  let recover msg p = rollback (P.label (PamlCore.Error.Message msg) p)
end

module Pratt (P : PamlCore.T.Parser) = struct
  type 'a parser = 'a P.t

  module U = PamlCore.Make.Utils (P)

  let ( let* ) = P.bind

  type 'a operator =
    | PrefixL of ('a -> 'a) parser
    | SuffixR of ('a -> 'a) parser
    | InfixL of ('a -> 'a -> 'a) parser
    | InfixR of ('a -> 'a -> 'a) parser

  let make term prefix infix suffix =
    (* Wyrażenie z operatorem prefiksowym. *)
    let rec climb_prefix _ =
      let* rbp, op = prefix in
      let* rhs = climb rbp in
      P.return (op rhs)
    (* Wyrażenie z operatorem sufiksowym. *)
    and climb_suffix bp lhs =
      let* lbp, op = suffix in
      let* _ = U.guard (lbp >= bp) in
      P.return (op lhs)
    (* Wyrażenie z operatorem infiksowym. *)
    and climb_infix bp lhs =
      let* lbp, rbp, op = infix in
      let* _ = U.guard (lbp >= bp) in
      let* rhs = climb rbp in
      P.return (op lhs rhs)
    (* Dowolne wyrażenie o mocy wiązania nie mniejszej niż bp. *)
    and climb bp =
      let* lhs = P.plus (climb_prefix bp) term in
      P.plus (loop bp lhs) (P.return lhs)
    and loop bp lhs =
      let* lhs = P.plus (climb_suffix bp lhs) (climb_infix bp lhs) in
      P.plus (loop bp lhs) (P.return lhs)
    in
    climb 0
  ;;

  let easy term table =
    let split i =
      let aux bp (pre, inf, suf) = function
        | PrefixL op -> (P.map op @@ fun op -> bp, op) :: pre, inf, suf
        | InfixL op -> pre, (P.map op @@ fun op -> bp - 1, bp, op) :: inf, suf
        | InfixR op -> pre, (P.map op @@ fun op -> bp, bp - 1, op) :: inf, suf
        | SuffixR op -> pre, inf, (P.map op @@ fun op -> bp, op) :: suf
      in
      List.fold_left (aux @@ ((i * 2) + 2)) ([], [], [])
    and join =
      let aux (pre, suf, inf) (p, s, i) = p @ pre, s @ suf, i @ inf in
      List.fold_left aux ([], [], [])
    in
    let pre, inf, suf = table |> List.mapi split |> join in
    make term (U.choice pre) (U.choice inf) (U.choice suf)
  ;;
end
