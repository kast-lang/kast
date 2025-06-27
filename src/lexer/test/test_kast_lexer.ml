open Std
module Token = Kast_token
module Lexer = Kast_lexer

module ExpectedToken = struct
  type t =
    | Ident of string
    | Punct of string
    | String of string
    | Eof

  type expected_token = t

  let print : formatter -> expected_token -> unit =
   fun fmt token ->
    match token with
    | Ident s -> fprintf fmt "@{<under>%a@}" String.print_maybe_escaped s
    | Punct s -> fprintf fmt "%a" String.print_maybe_escaped s
    | String s -> fprintf fmt "@{<green>%S@}" s
    | Eof -> fprintf fmt "@{<italic><eof>@}"

  let check : expected_token -> Token.t -> bool =
   fun expected token ->
    match (expected, token.shape) with
    | Ident expected, Ident token when token.raw = expected -> true
    | Ident _, _ -> false
    | Punct expected, Punct token when token.raw = expected -> true
    | Punct _, _ -> false
    | String expected, String token when token.contents = expected -> true
    | String _, _ -> false
    | Eof, Eof -> true
    | Eof, _ -> false
end

type expected_token = ExpectedToken.t

let test ~(source : string) ~(expected : expected_token list) : unit =
  let expected = expected @ [ Eof ] in
  let tokens =
    Lexer.read_all Lexer.default_rules
      { contents = source; filename = Special "test" }
  in
  if
    List.compare_lengths expected tokens <> 0
    || not
       @@ List.for_all
            (fun (expected, token) -> ExpectedToken.check expected token)
            (List.zip expected tokens)
  then
    let f fmt =
      fprintln fmt "@{<red>Test failed:@}";
      fprintln fmt "got: %a" (List.print Token.print) tokens;
      fprintln fmt "expected: %a" (List.print ExpectedToken.print) expected
    in
    raise @@ FailFormat f
;;

Printexc.record_backtrace true;
try
  test ~source:"" ~expected:[];
  test ~source:"hello, world"
    ~expected:[ Ident "hello"; Punct ","; Ident "world" ];
  test ~source:"std.print \"hello, world\""
    ~expected:[ Ident "std"; Punct "."; Ident "print"; String "hello, world" ]
with
| Failure s ->
    eprintln "@{<red>%s@}" s;
    Printexc.print_backtrace stderr;
    exit 1
| FailFormat f ->
    f Format.err_formatter;
    eprintln "";
    exit 1
