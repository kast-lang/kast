open Std
open Util

type token = Lexer.token

type simple = { token : token }
and complex = { name : string; children : ast tuple }
and ast = Simple of simple | Complex of complex

type t = ast

let rec print : formatter -> ast -> unit =
 fun fmt ast ->
  match ast with
  | Simple { token } -> Lexer.Token.print fmt token
  | Complex { name; children } ->
      fprintf fmt "%S %a" name (Tuple.print print) children
