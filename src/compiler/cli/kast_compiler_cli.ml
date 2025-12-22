open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
module Interpreter = Kast_interpreter
open Kast_types

module Args = struct
  type args = {
    path : Uri.t;
    output_type : output_type;
    no_std : bool;
  }

  and output_type = Ir

  type t = args

  let default_output_type = Ir

  let rec parse : string list -> args = function
    | [] ->
        { path = Uri.stdin; output_type = default_output_type; no_std = false }
    | [ path ] ->
        {
          path = Uri.file path;
          output_type = default_output_type;
          no_std = false;
        }
    | "--no-std" :: rest -> { (parse rest) with no_std = true }
    | "--output-type" :: output_type :: rest ->
        let output_type =
          match output_type with
          | "ir" -> Ir
          | _ -> fail "Unknown output type %S" output_type
        in
        { (parse rest) with output_type }
    | first :: _rest -> fail "Unexpected arg %S" first
end

let run : Args.t -> unit =
 fun { path; output_type; no_std } ->
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
      match output_type with
      | Ir -> println "%a" Expr.print_with_types expr)
