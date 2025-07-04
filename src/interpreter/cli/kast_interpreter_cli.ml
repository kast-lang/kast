open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
module Interpreter = Kast_interpreter
open Kast_types

module Args = struct
  type args = { path : Uri.t }
  type t = args

  let parse : string list -> args = function
    | [] -> { path = Uri.stdin }
    | [ path ] -> { path = Uri.file path }
    | first :: _rest -> fail "Unexpected arg %S" first
end

let eval_and : (value -> unit) -> Args.t -> unit =
 fun f { path } ->
  let source = Source.read path in
  let parsed = Parser.parse source Kast_default_syntax.ruleset in
  match parsed.ast with
  | None -> println "<none>"
  | Some ast ->
      let compiler = Compiler.default () in
      (* TODO *)
      let interpreter = compiler.interpreter in
      let expr : expr = Compiler.compile compiler Expr ast in
      let value : value = Interpreter.eval interpreter expr in
      f value

let eval = eval_and (fun value -> println "%a" Value.print value)
let run = eval_and ignore
