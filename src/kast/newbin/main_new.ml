open Std
module Kast = Kast_new.Kast
open Kast.Util

let main () =
  Random.self_init ();
  Kast.init ();
  let source = Source.read (Uri.file (Array.get Sys.argv 1)) in
  let parsed = Kast.Parser.parse source Kast.Syntax.Default.ruleset in
  let ast = parsed.ast |> Option.get in
  let compiler =
    Kast.Compiler.blank ~import_cache:(Kast.Compiler.Import.Cache.init ())
  in
  let compiled = compiler |> Kast.Compiler.compile Expr ast in
  let interpreter = Kast.Interpreter.default () in
  let result = interpreter |> Kast.Interpreter.Eval.expr compiled in
  println "eval result: %a" Kast.Value.print result

let handle_effects : 'a. (unit -> 'a) -> 'a =
 fun f ->
  try f () with
  | effect Kast.Error.T error, k ->
      Log.error (fun log -> log "%a" Kast.Error.print error);
      Effect.continue k ()
  | effect Kast_parser.Error.Error error, k ->
      Log.error (fun log -> log "%a" Kast_parser.Error.print error);
      Effect.continue k ()
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

let () =
  try handle_effects main with
  | Failure s ->
      Log.error (fun log -> log "@{<red>Failure@}: %s" s);
      Printexc.print_backtrace stderr;
      exit 1
  | FailFormat f ->
      Log.error (fun log -> log "@{<red>Fail@}: %t" f);
      Printexc.print_backtrace stderr;
      exit 1
