module Set = Set.Make (String)

type kind =
  | Expected of string list
  | Message of string
  | Other of exn

type t = (int * int) * kind

(* Ładne drukowanie błędów *)

let pp_expected f xs =
  let n = List.length xs in
  let iter = function
    | i when i = n - 1 -> Format.fprintf f "%s"
    | i when i = n - 2 -> Format.fprintf f "%s or "
    | _ -> Format.fprintf f "%s, "
  in
  List.iteri iter xs
;;

let pp_kind f = function
  | Expected [] -> Format.fprintf f "received invalid input"
  | Expected e -> Format.fprintf f "expected %a" pp_expected e
  | Message m -> Format.fprintf f "%s" m
  | Other err -> Printexc.to_string err |> Format.fprintf f "%s"
;;

let pp f ((l, r), e) = Format.fprintf f "(%d-%d) %a" l r pp_kind e

let merge (s1, e1) (s2, e2) =
  let union s1 s2 = Set.elements @@ Set.union (Set.of_list s1) (Set.of_list s2) in
  let aux (s1, e1) (s2, e2) =
    match e1, e2 with
    | Expected e1, Expected e2 -> max s1 s2, Expected (union e1 e2)
    | _, Other e2 -> s2, Other e2
    | Other e1, _ -> s1, Other e1
    | _, Message m2 -> s2, Message m2
    | Message m1, _ -> s1, Message m1
  in
  match compare (fst s1) (fst s2) with
  | r when r > 0 -> s1, e1
  | r when r < 0 -> s2, e2
  | _ -> aux (s1, e1) (s2, e2)
;;

let merge_opt e1 e2 =
  match e1, e2 with
  | None, None -> None
  | Some e1, None -> Some e1
  | None, Some e2 -> Some e2
  | Some e1, Some e2 -> Some (merge e1 e2)
;;

let empty sp = sp, Expected []

(* Tworzenie ładnego błędu *)

type pretty =
  { error : t;
    file : string;
    frame : string option;
    span : (int * int) * (int * int)
  }

exception ParseError of pretty list

let of_pretty { error; _ } = error
let pretty_start { span; _ } = fst span
let pretty_end { span; _ } = snd span

let get_location lines pos =
  let rec aux lineno pos = function
    | [] -> lineno, 0
    | line :: _ when String.length line >= pos -> lineno, pos
    | [ line ] -> lineno, String.length line
    | line :: lines -> aux (lineno + 1) (pos - String.length line - 1) lines
  in
  aux 0 pos lines
;;

let trunc ?(e = "..") l r s =
  let n, m = String.length s, String.length e in
  let e = if e = "" then "" else "\x1B[90m" ^ e ^ "\x1B[39m" in
  let le, l = if l <= 0 then "", 0 else e, l + m
  and re, r = if r >= n then "", n else e, r - m in
  le ^ String.sub s l (r - l) ^ re
;;

let pp_line f center lineno line =
  let line_fmt = format_of_string "\x1B[90m% 4d |\x1B[39m %s\n" in
  let l, r = center - 45, center + 45 in
  if String.trim line = ""
  then Format.fprintf f line_fmt (lineno + 1) "\x1B[90m<empty line>\x1B[39m"
  else Format.fprintf f line_fmt (lineno + 1) (line |> trunc l r)
;;

let pp_mark f center (mb, me) =
  let mark_fmt = format_of_string "\x1B[90m     |\x1B[39m \x1B[91m%s\x1B[39m\n" in
  let l, r = center - 45, center + 45 in
  let len = max (me - mb) 0 in
  Format.fprintf
    f
    mark_fmt
    (String.make mb ' ' ^ String.make (len + 1) '^' |> trunc ~e:"" l r)
;;

let pp_frame f lines (ap, bp) =
  let (alin, acol), (blin, bcol) = get_location lines ap, get_location lines (bp - 1) in
  let aux lineno line =
    let mark =
      match lineno with
      | ln when alin = ln && ln = blin -> Some (acol, bcol)
      | ln when alin < ln && ln < blin -> Some (0, String.length line)
      | ln when alin = ln -> Some (acol, String.length line)
      | ln when ln = blin -> Some (0, bcol)
      | _ -> None
    in
    if alin - 1 <= lineno && lineno <= blin + 1
    then begin
      let center = (acol + bcol) / 2 in
      pp_line f center lineno line;
      Option.iter (pp_mark f center) mark
    end
  in
  List.iteri aux lines
;;

let make_frame lines (ap, bp) =
  let pp f lines = pp_frame f lines (ap, bp) in
  Format.asprintf "%a" pp lines
;;

let pretty ?(file = "<unnamed>") source (((ap, bp), _) as error) =
  let lines = String.split_on_char '\n' source in
  let ap, bp = ap, max bp (ap + 1) in
  let (al, ac), (bl, bc) = get_location lines ap, get_location lines (bp - 1) in
  { error;
    file;
    span = (al + 1, ac + 1), (bl + 1, bc + 2);
    frame = (if bp - ap < 512 then Some (make_frame lines (ap, bp)) else None)
  }
;;

let pp_pretty f e =
  let (al, ac), (bl, bc) = e.span in
  if al != bl
  then Format.fprintf f "File %S, lines %d-%d:" e.file al bl
  else if bc - ac > 1
  then Format.fprintf f "File %S, line %d, characters %d-%d:" e.file al ac (bc - 1)
  else Format.fprintf f "File %S, line %d, character %d:" e.file al ac;
  match e.frame with
  | Some frame -> Format.fprintf f "\n\n%s\nError: %a" frame pp_kind (snd e.error)
  | None -> Format.fprintf f "%a" pp_kind (snd e.error)
;;

Printexc.register_printer
@@ function
| ParseError err ->
  Some (err |> List.map (Format.asprintf "%a" pp_pretty) |> String.concat "\n\n")
| _ -> None
