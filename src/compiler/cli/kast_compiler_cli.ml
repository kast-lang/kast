open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
open Kast_types

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
  let parsed = Parser.parse source Kast_default_syntax.ruleset in
  match parsed.ast with
  | None -> println "<none>"
  | Some ast ->
      let compiler = Compiler.init () in
      let expr : expr = Compiler.compile compiler Expr ast in
      println "%a" Expr.print_with_types expr
