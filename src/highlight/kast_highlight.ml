open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Parser = Kast_parser
module Ast = Kast_ast

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

let print_token (printer : printer) (f : printer -> Token.t -> unit)
    (token : Token.t spanned) : unit =
  move_to printer token.span.start;
  f printer token.value;
  printer.position <- token.span.finish

let print_comment (printer : printer) (comment : Lexer.Token.comment spanned) :
    unit =
  move_to printer comment.span.start;
  fprintf printer.fmt "@{<gray>%s@}" comment.value.raw;
  printer.position <- comment.span.finish

let print_keyword printer (token : Token.t) =
  match Lexer.Token.raw token with
  | Some raw -> fprintf printer.fmt "@{<magenta>%s@}" raw
  | None -> ()

let print_value printer (token : Token.t) =
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

let print (fmt : formatter) ({ ast; trailing_comments; eof } : Parser.result) :
    unit =
  let printer = { fmt; position = Position.beginning } in
  (match ast with
  | Some ast -> print_ast printer ast
  | None -> ());
  trailing_comments |> List.iter (fun comment -> print_comment printer comment);
  move_to printer eof
