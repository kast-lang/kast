open Std
open Kast_util
module Parser = Kast_parser
module Ast = Kast_ast

module Args = struct
  type args = { path : path }
  type t = args

  let parse : string list -> args = function
    | [] -> { path = Stdin }
    | [ path ] -> { path = File path }
    | first :: _rest -> fail "Unexpected arg %S" first
end

let run : Args.t -> unit =
 fun { path } ->
  let source = Source.read path in
  let { ast; trailing_comments = _; eof = _ } : Parser.result =
    Parser.parse source Kast_default_syntax.ruleset
  in
  match ast with
  | Some ast -> println "%a" Ast.print ast
  | None -> println "@{<dim;italic><nothing>@}"
