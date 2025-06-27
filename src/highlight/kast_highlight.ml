open Std
open Util

module Args = struct
  type args = { path : path }
  type t = args

  let parse : string list -> args = function
    | [] -> { path = Stdin }
    | [ path ] -> { path = File path }
    | first :: _rest -> fail "Unexpected arg %S" first
end

type args = Args.t

type printer = {
  fmt : formatter;
  mutable position : position;
}

let print_char printer c =
  printer.position <- Position.advance c printer.position;
  fprintf printer.fmt "%c" c

(* TODO actual whitespace tokens? *)
let move_to (printer : printer) (position : position) : unit =
  while printer.position.line < position.line do
    print_char printer '\n'
  done;
  while printer.position.column < position.column do
    print_char printer ' '
  done

let print_token (printer : printer) (f : printer -> Lexer.token -> unit)
    (token : Lexer.token spanned) : unit =
  move_to printer token.span.start;
  f printer token.value;
  printer.position <- token.span.finish

let print_comment (printer : printer) (comment : Lexer.Token.comment spanned) :
    unit =
  move_to printer comment.span.start;
  fprintf printer.fmt "@{<gray>%s@}" comment.value.raw;
  printer.position <- comment.span.finish

let print_keyword printer (token : Lexer.token) =
  match Lexer.Token.raw token with
  | Some raw -> fprintf printer.fmt "@{<magenta>%s@}" raw
  | None -> ()

let print_value printer (token : Lexer.token) =
  let fmt = printer.fmt in
  match token with
  | Ident { raw; _ } -> fprintf fmt "%s" raw
  | String { raw; _ } -> fprintf fmt "@{<green>%s@}" raw
  | Number { raw; _ } -> fprintf fmt "@{<italic>%s@}" raw
  | Punct { raw; _ } -> fprintf fmt "%s" raw
  | Comment _ -> unreachable "<comment> value??"
  | Eof -> unreachable "<eof> value??"

let rec print_ast (printer : printer) (ast : Ast.t) : unit =
  match ast.shape with
  | Simple { comments_before; token } ->
      comments_before |> List.iter (print_comment printer);
      print_token printer print_value token
  | Complex { parts; _ } ->
      parts
      |> List.iter (function
           | Ast.Value ast -> print_ast printer ast
           | Ast.Keyword token -> print_token printer print_keyword token
           | Ast.Comment comment -> print_comment printer comment)

let run : args -> unit =
 fun { path } ->
  let source = Source.read path in
  let lexer = Lexer.init Lexer.default_rules source in
  let { ast; trailing_comments } : Parser.result =
    Parser.parse_with_lexer lexer Default_syntax.ruleset
  in
  let eof_token = Lexer.peek lexer in
  let printer = { fmt = Format.std_formatter; position = Position.beginning } in
  (match ast with
  | Some ast -> print_ast printer ast
  | None -> ());
  trailing_comments |> List.iter (fun comment -> print_comment printer comment);
  move_to printer eof_token.span.start
