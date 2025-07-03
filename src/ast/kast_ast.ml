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
  | Group of group

and complex = {
  rule : Syntax.rule;
  root : group;
}

and group = {
  (* TODO rule is only None for root *)
  rule : Syntax.Rule.group option;
  parts : part list;
  children : child tuple;
}

and child =
  | Ast of ast
  | Group of group

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

and print_child : formatter -> child -> unit =
 fun fmt -> function
  | Ast ast -> print fmt ast
  | Group group -> print_group fmt group

and print_group : formatter -> group -> unit =
 fun fmt { rule = _; parts = _; children } ->
  Tuple.print print_child fmt children

and print_shape : formatter -> shape -> unit =
 fun fmt -> function
  | Simple { comments_before = _; token } -> Token.Shape.print fmt token.shape
  | Complex { rule; root } ->
      fprintf fmt "@{<magenta>%a@} %a" String.print_maybe_escaped rule.name
        print_group root
  | Syntax { comments_before = _; mode; value_after; tokens = _ } -> (
      SyntaxMode.print fmt mode;
      match value_after with
      | None -> ()
      | Some value -> fprintf fmt "\n%a" print value)

and print_shape_short : formatter -> shape -> unit =
 fun fmt -> function
  | Simple { comments_before = _; token } -> Token.Shape.print fmt token.shape
  | Complex { rule; root = _ } ->
      fprintf fmt "@{<magenta>%a@}" String.print_maybe_escaped rule.name
  | Syntax { comments_before = _; mode = _; value_after = _; tokens = _ } ->
      fprintf fmt "<syntax>"

module Shape = struct
  type t = shape

  let print = print_shape
  let print_short = print_shape_short
end

module Child = struct
  type t = child

  let expect_ast = function
    | Ast ast -> ast
    | Group _ -> fail "expected ast, got group"

  let expect_group = function
    | Ast _ -> fail "expected group, got ast"
    | Group group -> group

  let print = print_child
end

module Part = struct
  type t = part

  let print =
   fun fmt -> function
    | Comment _ -> fprintf fmt "<comment>"
    | Keyword token -> fprintf fmt "keyword %a" Token.print token
    | Value value -> print fmt value
    | Group group -> print_group fmt group
end
