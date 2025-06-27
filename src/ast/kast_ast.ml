open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer

type simple = {
  comments_before : Lexer.Token.comment spanned list;
  token : Token.t spanned;
}

type part =
  | Comment of Lexer.Token.comment spanned
  | Value of ast
  | Keyword of Token.t spanned

and complex = {
  name : string;
  parts : part list;
  children : ast tuple;
}

and shape =
  | Simple of simple
  | Complex of complex

and ast = {
  shape : shape;
  span : span;
}

type t = ast

let rec print : formatter -> ast -> unit =
 fun fmt { shape; span } ->
  fprintf fmt "%a @{<dim>at %a@}" print_shape shape Span.print span

and print_shape : formatter -> shape -> unit =
 fun fmt -> function
  | Simple { comments_before = _; token } -> Lexer.Token.print fmt token.value
  | Complex { name; parts = _; children } ->
      fprintf fmt "@{<magenta>%a@} %a" String.print_maybe_escaped name
        (Tuple.print print) children

module Kind = struct
  type t = shape

  let print = print_shape
end
