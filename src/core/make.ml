let debug_indent = ref 0

module Parser (I : Types.Input) = struct
  module I = I
  module Data = Map.Make (Int)

  type state =
    { input : I.t;
      break : bool;
      data : Obj.t Data.t
    }

  let st_inp input = { input; break = false; data = Data.empty }
  let err_inp a = Error.empty @@ (I.posl a, I.posl a)
  let err_inps a b = Error.empty @@ (I.posl a, I.posr b)

  type 'a t = state -> 'a option * state * Error.t option

  let run p input =
    match p (st_inp input) with
    | Some x, _, _ -> Ok x
    | None, _, Some e -> Error e
    | None, _, None -> Error (Error.empty @@ I.span input)
  ;;

  let return x st = Some x, st, None

  let map p f st =
    let rx, tt, err = p st in
    Option.map f rx, tt, err
  ;;

  let bind : 'a t -> ('a -> 'b t) -> 'b t =
   fun p f st ->
    match p st with
    | None, t1, e1 -> None, t1, e1
    | Some x, t1, e1 ->
      let ry, t2, e2 = f x t1 in
      ry, t2, Error.merge_opt e1 e2
 ;;

  let plus p r st =
    match p st with
    | Some x, t1, e1 -> Some x, t1, e1
    | None, t1, e1 when t1.break -> None, t1, e1
    | None, _, e1 ->
      let ry, t2, e2 = r st in
      ry, t2, Error.merge_opt e1 e2
  ;;

  let empty st = None, st, Some (err_inp st.input)
  let input st = Some st.input, st, None

  let any st =
    match I.next st.input with
    | Some (x, input) -> Some x, { st with input }, Some (err_inps st.input input)
    | None -> None, st, Some (err_inp st.input)
  ;;

  let take n st =
    match I.take n st.input with
    | Some (xs, input) -> Some xs, { st with input }, Some (err_inps st.input input)
    | None -> None, st, Some (err_inp st.input)
  ;;

  let eof st = (if I.eof st.input then Some () else None), st, Some (err_inp st.input)

  (* Obsługa błędów *)

  let throw error st = None, st, Some error

  let error error p st =
    let r, tt, _ = p st in
    r, tt, Some error
  ;;

  let label error p st =
    let rx, tt, e2 = p st in
    let e1 = (I.posl st.input, I.posr tt.input), error in
    match e1, e2 with
    | e1, None -> rx, tt, Some e1
    | e1, Some (_, Error.Expected []) -> rx, tt, Some e1
    | e1, Some e2 ->
      let l1, ((l2, _), _) = I.posl st.input, e2 in
      let err = if l1 < l2 then e2 else e1 in
      rx, tt, Some err
  ;;

  let hidden p st =
    let r, tt, _ = p st in
    r, tt, None
  ;;

  let catch p st =
    match p st with
    | Some x, tt, _ -> Some (Ok x), tt, None
    | None, _, Some err -> Some (Error err), st, None
    | None, tt, None -> Some (Error (err_inps st.input tt.input)), st, None
  ;;

  let peek p st =
    let r, _, err = p st in
    r, st, err
  ;;

  let defer f st = f () st
  let key_total = ref 0

  let state x =
    incr key_total;
    let key = !key_total in
    let get st =
      match Data.find_opt key st.data with
      | Some x -> Some (Obj.obj x), st, None
      | None -> Some x, st, None
    and set x st =
      let data = Data.add key (Obj.repr x) st.data in
      Some (), { st with data }, None
    in
    get, set
  ;;

  (* Debugowanie *)

  let tap enter leave p st =
    let x = enter st.input in
    let res, tt, err = p st in
    let error = Option.value ~default:(err_inps st.input tt.input) err in
    leave tt.input x @@ Option.fold ~none:(Result.error error) ~some:Result.ok res;
    res, tt, err
  ;;
end

module Utils (P : Types.Parser) = struct
  type 'a parser = 'a P.t

  module I = P.I

  let ( let* ) = P.bind

  (* Dane wejściowe *)

  let index = P.map P.input I.index
  let posl = P.map P.input I.posl
  let posr = P.map P.input I.posr

  let with_len p =
    let* il = index in
    let* x = p in
    let* ir = index in
    P.return (ir - il, x)
  ;;

  let with_elts p =
    let* n, x = P.peek @@ with_len p in
    let* xs = P.take n in
    P.return (xs, x)
  ;;

  let with_span p =
    let* sl = posl in
    let* x = p in
    let* sr = posr in
    P.return ((sl, sr), x)
  ;;

  let capture p = P.map (with_elts p) fst

  (* Obsługa błędów *)

  let expect msg p = P.label (Error.Expected [ msg ]) p
  let expects msgs p = P.label (Error.Expected msgs) p
  let message msg p = P.label (Error.Message msg) p

  (* Funkcje pomocnicze *)

  let map_with f p = P.map p f
  let bind_with f p = P.bind p f

  let rec choice = function
    | [] -> P.empty
    | [ p ] -> p
    | p :: ps -> P.plus p (choice ps)
  ;;

  let rec chain = function
    | [] -> P.return []
    | [ p ] -> P.map p @@ fun x -> [ x ]
    | p :: ps ->
      let* x = p in
      P.map (chain ps) @@ fun xs -> x :: xs
  ;;

  let first p r = P.bind p @@ fun x -> P.map r @@ Fun.const x
  let second p r = P.bind p @@ Fun.const r
  let between l p r = second l (first r p)

  (* Predykaty i weryfikacja *)

  let sats f =
    let* x = P.any in
    if f x then P.return x else P.empty
  ;;

  let non p =
    let* x = P.catch p in
    if Result.is_ok x then P.empty else P.return ()
  ;;

  let guard b = if b then P.return () else P.empty

  let verify f p =
    let* sp, x = with_span p in
    if f x then P.return x else P.throw (Error.empty sp)
  ;;

  let nonempty p =
    let* n, x = with_len p in
    if n = 0 then P.empty else P.return x
  ;;

  (* Obsługa wbudowanych typów *)

  let void p = P.map p (Fun.const ())
  let option p = P.plus (P.map p Option.some) (P.return Option.none)
  let default x p = P.plus p (P.return x)
  let either p r = P.plus (P.map p Either.left) (P.map r Either.right)

  let pair p r =
    let* x = p in
    let* y = r in
    P.return (x, y)
  ;;

  (* Repetycja *)

  let rec many p = P.plus (some p) (P.return [])

  and some p =
    let* x = nonempty p in
    P.map (many p) @@ fun xs -> x :: xs
  ;;

  let until r p = many @@ second (non r) p

  let rec count n p =
    match n with
    | n when n <= 0 -> P.return []
    | n ->
      let* x = p in
      P.map (count (n - 1) p) @@ fun xs -> x :: xs
  ;;

  let sep r p =
    let* x = p in
    let* xs = many (second r p) in
    P.return (x :: xs)
  ;;

  let sept r p =
    let* xs = sep r p in
    let* _ = option r in
    P.return xs
  ;;

  (* Debugowanie *)

  let debug_enter, debug_leave =
    let indent () = String.make (!debug_indent * 4) ' ' in
    let enter label msg =
      Format.eprintf "\x1B[90m%s> %s:\x1B[39m %s\n%!" (indent ()) label msg;
      incr debug_indent
    and leave label msg =
      decr debug_indent;
      Format.eprintf "\x1B[90m%s< %s:\x1B[39m %s\n%!" (indent ()) label msg
    in
    enter, leave
  ;;

  let trace label =
    let status = function
      | Ok _ -> "ok"
      | Error err -> Format.asprintf "\x1B[91m%a\x1B[39m" Error.pp err
    in
    let enter _ = debug_enter label ""
    and leave _ _ res = debug_leave label (status res) in
    P.tap enter leave
  ;;
end

module Ops (P : Types.Parser) = struct
  module U = Utils (P)

  type 'a parser = 'a P.t

  let ( let* ) = P.bind
  let ( and* ) = U.pair
  let ( >>= ) = P.bind
  let ( <&> ) = P.map
  let ( <|> ) = P.plus
  let ( <$> ) f p = P.map p f
  let ( <$ ) x p = P.map p (Fun.const x)
  let ( $> ) p x = P.map p (Fun.const x)
  let ( <*> ) p r = P.bind p @@ fun f -> P.map r f
  let ( <* ) = U.first
  let ( *> ) = U.second
  let ( <??> ) p msgs = U.expects msgs p
  let ( <?> ) p msg = U.expect msg p
  let ( !: ) = P.defer
  let ( !? ) = U.option
  let ( !* ) = U.many
  let ( !+ ) = U.some
end
