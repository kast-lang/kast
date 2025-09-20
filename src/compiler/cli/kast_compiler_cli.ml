open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
open Kast_types

module Args = struct
  type args = {
    path : Uri.t;
    output_type : output_type;
  }

  and output_type =
    | Ir
    | Ocaml

  type t = args

  let default_output_type = Ocaml

  let rec parse : string list -> args = function
    | [] -> { path = Uri.stdin; output_type = default_output_type }
    | [ path ] -> { path = Uri.file path; output_type = default_output_type }
    | "--output-type" :: output_type :: rest ->
        let output_type =
          match output_type with
          | "ir" -> Ir
          | "ocaml" -> Ocaml
          | _ -> fail "Unknown output type %S" output_type
        in
        { (parse rest) with output_type }
    | first :: _rest -> fail "Unexpected arg %S" first
end

let run : Args.t -> unit =
 fun { path; output_type } ->
  let source = Source.read path in
  let parsed = Parser.parse source Kast_default_syntax.ruleset in
  match parsed.ast with
  | None -> println "<none>"
  | Some ast -> (
      let compiler = Compiler.default () in
      let expr : expr = Compiler.compile compiler Expr ast in
      match output_type with
      | Ir -> println "%a" Expr.print_with_types expr
      | Ocaml ->
          let ocaml_ast = Kast_transpiler_ocaml.transpile_expr expr in
          println "%a" Kast_transpiler_ocaml.OcamlAst.print ocaml_ast)
