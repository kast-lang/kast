open Kast;;

Printexc.record_backtrace true

module ExpectedToken = struct
  type t = Ident of string | Punct of string | Eof
  type expected_token = t

  let print : formatter -> expected_token -> unit =
   fun fmt token ->
    match token with
    | Ident s -> fprintf fmt "%S" s
    | Punct s -> fprintf fmt "%S" s
    | Eof -> fprintf fmt "<eof>"

  let check : expected_token -> token -> bool =
   fun expected token ->
    match (expected, token) with
    | Ident expected, Ident token when token.raw = expected -> true
    | Ident _, _ -> false
    | Punct expected, Punct token when token.raw = expected -> true
    | Punct _, _ -> false
    | Eof, Eof -> true
    | Eof, _ -> false
end

type expected_token = ExpectedToken.t

let test ~(source : string) ~(expected : expected_token list) : unit =
  let expected = expected @ [ Eof ] in
  let tokens = Lexer.read_all { contents = source; filename = "<test>" } in
  let tokens : token list =
    List.map (fun (spanned : _ spanned) -> spanned.value) tokens
  in
  if
    not
    @@ List.for_all
         (fun (expected, token) -> ExpectedToken.check expected token)
         (List.zip expected tokens)
  then (
    eprintln "got     : %a" (List.print Token.print) tokens;
    eprintln "expected: %a" (List.print ExpectedToken.print) expected;
    failwith "test failed")
;;

test ~source:"" ~expected:[];;

test ~source:"hello, world"
  ~expected:[ Ident "hello"; Punct ","; Ident "world" ]
