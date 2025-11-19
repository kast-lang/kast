open Std
include Kast_util
module Lexer = Kast_lexer
module Ast = Kast_ast
module Parser = Kast_parser
module Highlight = Kast_highlight
module Interpreter = Kast_interpreter
module Compiler = Kast_compiler
module Fmt = Kast_fmt
include Kast_types

let handle_effects : 'a. (unit -> 'a) -> 'a =
 fun f ->
  try f () with
  | effect Kast_interpreter.Error.Error error, k ->
      Log.error (fun log -> log "%a" Kast_interpreter.Error.print error);
      Effect.continue k ()
  | effect Kast_compiler.Error.Error error, k ->
      Log.error (fun log -> log "%a" Kast_compiler.Error.print error);
      Effect.continue k ()
  | effect Kast_inference_base.Error.Error error, k ->
      Log.error (fun log -> log "%a" Kast_inference_base.Error.print error);
      Effect.continue k ()
  | effect Kast_parser.Error.Error error, k ->
      Log.error (fun log -> log "%a" Kast_parser.Error.print error);
      Effect.continue k ()
  | effect Kast_compiler.Effect.FileIncluded _, k -> Effect.Deep.continue k ()
  | effect Kast_compiler.Effect.FileImported _, k -> Effect.Deep.continue k ()
  | effect Kast_compiler.Effect.FileStartedProcessing _, k ->
      Effect.Deep.continue k ()
  | effect Kast_compiler.Effect.FindStd, k ->
      let stdlib_path =
        match Sys.getenv_opt "KAST_STD" with
        | Some path -> path
        | None -> Sys.getcwd () ^ "/std"
      in
      Effect.Deep.continue k (Uri.file stdlib_path)
  | effect Source.Read uri, k ->
      Effect.continue_with k (fun () ->
          match Uri.scheme uri with
          | Some "file" ->
              let path = Uri.path uri in
              read_from_filesystem path
          | Some "stdin" -> In_channel.input_all stdin
          | scheme ->
              fail "unsupported uri scheme %a"
                (Option.print String.print_dbg)
                scheme)
  | effect Interpreter.Natives.Input s, k ->
      print_string s;
      flush stdout;
      let line = read_line () in
      Effect.continue k line
  | effect Compiler.Scope.AwaitUpdate _, k -> Effect.continue k false
