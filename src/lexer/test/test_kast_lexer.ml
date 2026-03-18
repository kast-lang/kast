open Std
module Token = Kast_token
module Lexer = Kast_lexer

module ExpectedToken = struct
  type t =
    | Ident of string
    | Punct of string
    | Number of string
    | String of string_part list
    | Eof

  and string_part =
    | Content of string
    | Interpolate of t list

  type expected_token = t

  let rec print : formatter -> expected_token -> unit =
    fun fmt token ->
    match token with
    | Ident s -> fprintf fmt "@{<under>%a@}" String.print_maybe_escaped s
    | Punct s -> fprintf fmt "%a" String.print_maybe_escaped s
    | Number s -> fprintf fmt "@{<italic>%a@}" String.print s
    | String parts ->
      parts
      |> List.iter (function
        | Content s -> fprintf fmt "@{<green>%a@}" String.print_debug s
        | Interpolate tokens -> tokens |> List.iter (print fmt))
    | Eof -> fprintf fmt "@{<italic><eof>@}"
  ;;

  let rec check : expected_token -> Token.t -> bool =
    fun expected token ->
    match expected, token.shape with
    | Ident expected, Ident token when token.raw = expected -> true
    | Ident _, _ -> false
    | Punct expected, Punct token when token.raw = expected -> true
    | Punct _, _ -> false
    | Number a, Number b when String.equal a b.raw -> true
    | Number _, _ -> false
    | String expected, String token ->
      (try
         List.zip expected token.parts
         |> List.iter (fun (expected, part) ->
           match expected, part with
           | Content a, Token.Types.Content b when String.equal a b.raw -> ()
           | Interpolate a, Token.Types.Interpolate b ->
             List.zip a b
             |> List.iter (fun (expected, token) ->
               if not (check expected token) then raise <| Invalid_argument ":)")
           | _, _ -> raise <| Invalid_argument ":)");
         true
       with
       | Invalid_argument _ -> false)
    | String _, _ -> false
    | Eof, Eof -> true
    | Eof, _ -> false
  ;;
end

type expected_token = ExpectedToken.t

let test ~(source : string) ~(expected : expected_token list) : unit =
  let expected = expected @ [ Eof ] in
  let tokens =
    Lexer.read_all
      Lexer.default_rules
      { contents = source; uri = Uri.of_string "ocaml:test" }
  in
  if
    List.compare_lengths expected tokens <> 0
    || not
       <| List.for_all
            (fun (expected, token) -> ExpectedToken.check expected token)
            (List.zip expected tokens)
  then (
    let f fmt =
      fprintln fmt "@{<red>Test failed:@}";
      fprintln fmt "got: %a" (List.print Token.print) tokens;
      fprintln fmt "expected: %a" (List.print ExpectedToken.print) expected
    in
    raise <| FailFormat f)
;;

Printexc.record_backtrace true;
try
  test ~source:"" ~expected:[];
  test ~source:"hello, world" ~expected:[ Ident "hello"; Punct ","; Ident "world" ];
  test
    ~source:"std.print \"hello, world\""
    ~expected:[ Ident "std"; Punct "."; Ident "print"; String [ Content "hello, world" ] ];
  test
    ~source:"std.print \"hello, \\(2 + 2)\""
    ~expected:
      [ Ident "std"
      ; Punct "."
      ; Ident "print"
      ; String [ Content "hello, "; Interpolate [ Number "2"; Punct "+"; Number "2" ] ]
      ]
with
| Failure s ->
  eprintln "@{<red>%s@}" s;
  Printexc.print_backtrace stderr;
  exit 1
| FailFormat f ->
  f Format.err_formatter;
  eprintln "";
  exit 1
