open Std
open Util

type token = Lexer.token

type simple = { token : token }

and complex = {
  name : string;
  children : ast tuple;
}

and kind =
  | Simple of simple
  | Complex of complex

and ast = {
  kind : kind;
  span : span;
}

type t = ast

let rec print : formatter -> ast -> unit =
 fun fmt { kind; span } ->
  fprintf fmt "%a @{<dim>at %a@}" print_kind kind Span.print span

and print_kind : formatter -> kind -> unit =
 fun fmt -> function
  | Simple { token } -> Lexer.Token.print fmt token
  | Complex { name; children } ->
      fprintf fmt "%a %a" String.print_maybe_escaped name (Tuple.print print)
        children

module Kind = struct
  type t = kind

  let print = print_kind
end
