open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
module Interpreter = Kast_interpreter
open Kast_types
module Target = Kast_compiler_targets.Target

module Args = struct
  type args = {
    path : Uri.t;
    target : Target.t;
    no_std : bool;
  }

  type t = args

  let default_target = Target.Ir

  let rec parse : string list -> args = function
    | [] -> { path = Uri.stdin; target = default_target; no_std = false }
    | [ path ] ->
        { path = Uri.file path; target = default_target; no_std = false }
    | "--no-std" :: rest -> { (parse rest) with no_std = true }
    | "--target" :: target :: rest ->
        let target = Target.parse target in
        { (parse rest) with target }
    | first :: _rest -> fail "Unexpected arg %S" first
end

let run : Args.t -> unit =
 fun { path; target; no_std } ->
  let source = Source.read path in
  let parsed = Parser.parse source Kast_default_syntax.ruleset in
  match parsed.ast with
  | None -> println "<none>"
  | Some ast -> (
      let compiler =
        if no_std then
          let interpreter = Interpreter.default (Uri path) in
          Compiler.init
            ~import_cache:(Compiler.init_import_cache ())
            ~compile_for:interpreter
        else Compiler.default (Uri source.uri) ()
      in
      let expr : expr = Compiler.compile compiler Expr ast in
      match target with
      | Ir -> println "%a" Expr.print_with_types expr
      | JavaScript ->
          let transpiled : Kast_transpiler_javascript.result =
            Kast_transpiler_javascript.transpile_expr
              ~state:compiler.interpreter ~span:ast.span expr
          in
          println "%t" transpiled.print)
