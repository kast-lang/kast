open Std
open Kast_util
module Parser = Kast_parser
module Ast = Kast_ast

module Args = struct
  type args = { path : Uri.t }
  type t = args

  let parse : string list -> args = function
    | [] -> { path = Uri.stdin }
    | [ path ] -> { path = Uri.file path }
    | first :: _rest -> fail "Unexpected arg %S" first
  ;;
end

let run : Args.t -> unit =
  fun { path } ->
  let source = Source.read path in
  let { ast; trailing_comments = _; eof = _ } : Parser.result =
    Parser.parse source Kast_default_syntax.ruleset
  in
  println "%a" Ast.print ast
;;
