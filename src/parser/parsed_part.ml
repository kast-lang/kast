open Std
open Kast_util
module Token = Kast_token
module Ast = Kast_ast.T

type t =
  | Comment of Token.comment
  | Keyword of Token.t
  | Value of Ast.t

type parsed_part = t

let print : formatter -> parsed_part -> unit =
  fun fmt -> function
  | Comment comment -> fprintf fmt "comment %a" Token.Shape.print (Comment comment.shape)
  | Keyword token -> Token.print fmt token
  | Value ast -> Ast.print fmt ast
;;
