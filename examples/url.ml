(* https://url.spec.whatwg.org *)

module Parser = struct
  open Paml.Strings.Parser
  open Paml.Strings.Utils
  open Paml.Strings.Ops

  type url =
    { scheme : string;
      host : string;
      port : int option;
      path : string list;
      query : string option;
      frag : string option
    }

  let url_char = alphanum <|> char_of "!$&'()*+,-.;=@_~?"
  and path_char = alphanum <|> char_of "!$&'()*+,-.:;=@_~"

  let scheme = expect "scheme" @@ capture @@ some @@ (alphanum <|> char_of "+-.")
  let host = expect "host" @@ capture @@ some url_char
  let port = expect "port" @@ map_with int_of_string @@ capture @@ some num
  let path = expect "path" @@ sep (char '/') @@ capture @@ many path_char
  let query = expect "query" @@ capture @@ many url_char
  let frag = expect "fragment" @@ capture @@ many url_char

  let url =
    let* scheme = scheme <* string "://" in
    let* host in
    let* port = option (char ':' *> port) in
    let* path = default [] (char '/' *> path) in
    let* query = option (char '?' *> query) in
    let* frag = option (char '#' *> frag) in
    let* _ = expect "eof" eof in
    return { scheme; host; port; path; query; frag }
  ;;

  let parse = run_string_t url
end

let _ =
  if Array.length Sys.argv < 2
  then begin
    Format.eprintf "Usage: url.exe <url>\n";
    exit 3
  end;
  let url = Parser.parse ~file:"<input>" Sys.argv.(1) in
  Format.printf "scheme:   %s\n" url.scheme;
  Format.printf "host:     %s\n" url.host;
  Format.printf "port:     %a\n" (Format.pp_print_option Format.pp_print_int) url.port;
  Format.printf "path:     ";
  List.iter (Format.printf "/%s") url.path;
  Format.printf "\n";
  Format.printf "query:    %a\n" (Format.pp_print_option Format.pp_print_string) url.query;
  Format.printf "fragment: %a\n" (Format.pp_print_option Format.pp_print_string) url.frag
;;
