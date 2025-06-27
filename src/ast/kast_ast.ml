open Std
open Kast_util
module Token = Kast_token
module Syntax = Kast_syntax

type simple = {
  comments_before : Token.comment list;
  token : Token.t;
}

module SyntaxMode = struct
  type t =
    | Define of Syntax.rule
    | FromScratch

  let print fmt mode =
    fprintf fmt "@{<yellow>";
    (match mode with
    | Define rule -> fprintf fmt "syntax %S" rule.name
    | FromScratch -> fprintf fmt "syntax from_scratch");
    fprintf fmt "@}"
end

type part =
  | Comment of Token.comment
  | Value of ast
  | Keyword of Token.t

and complex = {
  rule : Syntax.rule;
  parts : part list;
  children : ast tuple;
}

and syntax = {
  comments_before : Token.comment list;
  mode : SyntaxMode.t;
  tokens : Token.t list; (* TODO more typed parts so we can highlight better? *)
  value_after : ast option;
}

and shape =
  | Simple of simple
  | Complex of complex
  | Syntax of syntax

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
  | Simple { comments_before = _; token } -> Token.Shape.print fmt token.shape
  | Complex { rule; parts = _; children } ->
      fprintf fmt "@{<magenta>%a@} %a" String.print_maybe_escaped rule.name
        (Tuple.print print) children
  | Syntax { comments_before = _; mode; value_after; tokens = _ } -> (
      SyntaxMode.print fmt mode;
      match value_after with
      | None -> ()
      | Some value -> fprintf fmt "\n%a" print value)

module Kind = struct
  type t = shape

  let print = print_shape
end
