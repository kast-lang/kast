open Std
open Kast_util
module Parser = Kast_parser
module Ast = Kast_ast
open Kast_types
module Compiler = Kast_compiler

type file_state = {
  uri : Lsp.Uri.t;
  parsed : Parser.result option;
  compiled : expr option;
}

let process_file (uri : Lsp.Uri.t) (source : source) : file_state =
  let parsed =
    try
      let result = Parser.parse source Kast_default_syntax.ruleset in
      Some result
    with _ -> None
  in
  let ast = Option.bind parsed (fun ({ ast; _ } : Parser.result) -> ast) in
  let compiled =
    Option.bind ast (fun ast ->
        let interpreter = Kast_interpreter.default () in
        let compiler = Compiler.init ~compile_for:interpreter in
        try Some (Compiler.compile compiler Expr ast) with _ -> None)
  in
  { uri; parsed; compiled }
