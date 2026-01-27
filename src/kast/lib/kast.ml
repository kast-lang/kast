open Std
include Kast_util
module Lexer = Kast_lexer
module Parser = Kast_parser
module Highlight = Kast_highlight
module Interpreter = Kast_interpreter
module Compiler = Kast_compiler
module Inference = Kast_inference
module Fmt = Kast_fmt
include Kast_types

let handle_effects : stop_on_error:bool -> (unit -> unit) -> unit =
  fun ~stop_on_error f ->
  let handle_error k =
    if stop_on_error
    then Effect.continue_with k (fun () -> fail "stopping on error")
    else Effect.continue k ()
  in
  try f () with
  | Kast_interpreter.Natives.Panic s ->
    Log.error (fun log -> log "panic: %s" s);
    exit (-1)
  | effect Kast_inference_completion.Error.Error error, k ->
    Log.error (fun log -> log "%a" Kast_inference_completion.Error.print error);
    handle_error k
  | effect Kast_interpreter.Error.Error error, k ->
    Log.error (fun log -> log "%a" Kast_interpreter.Error.print error);
    handle_error k
  | effect Kast_compiler.Error.Error error, k ->
    Log.error (fun log -> log "%a" Kast_compiler.Error.print error);
    handle_error k
  | effect Kast_inference_base.Error.Error error, k ->
    Log.error (fun log -> log "%a" Kast_inference_base.Error.print error);
    handle_error k
  | effect Kast_parser.Error.Error error, k ->
    Log.error (fun log -> log "%a" Kast_parser.Error.print error);
    handle_error k
  | effect Kast_compiler.Effect.FileIncluded _, k -> Effect.Deep.continue k ()
  | effect Kast_compiler.Effect.FileImported _, k -> Effect.Deep.continue k ()
  | effect Kast_compiler.Effect.FileStartedProcessing _, k -> Effect.Deep.continue k ()
  | effect Kast_compiler.Effect.FindStd, k ->
    let stdlib_uri =
      match Sys.getenv_opt "KAST_STD" with
      | Some path ->
        Log.trace (fun log -> log "Using KAST_STD env var = %a" String.print_debug path);
        Uri.file path
      | None ->
        (match [%getenv "KAST_STD"] with
         | "" -> Uri.file "std"
         | path ->
           let path = Uri.of_string path in
           let bin_path = Sys.executable_name in
           Log.trace (fun log ->
             log
               "Using KAST_STD env var (comptime) = %a, bin path = %a"
               Uri.print_full
               path
               String.print_debug
               bin_path);
           Uri.resolve "" (Uri.file bin_path) path)
    in
    Effect.Deep.continue k stdlib_uri
  | effect Source.Read uri, k ->
    Effect.continue_with k (fun () ->
      match Uri.scheme uri with
      | Some "file" ->
        let path = Uri.file_path uri in
        read_from_filesystem path
      | Some "stdin" -> In_channel.input_all stdin
      | scheme ->
        fail "unsupported uri scheme %a" (Option.print String.print_debug) scheme)
  | effect Interpreter.Natives.Io.Input s, k ->
    print_string s;
    flush stdout;
    let line = read_line () in
    Effect.continue k line
  | effect Interpreter.AwaitCompiled _, k ->
    Effect.continue_with k (fun () -> fail "Can't await fn to be compiled")
  | effect Compiler.Scope.AwaitUpdate _, k -> Effect.continue k false
  | effect Interpreter.Scope.AwaitUpdate (_name, _scope), k -> Effect.continue k false
  | effect Inference.Var.AwaitUpdate var, k ->
    (match Inference.Var.inferred_or_default var with
     | Some _ -> Effect.continue k true
     | None ->
       Effect.continue_with k (fun () ->
         fail
           "var at %a is not inferred and can't be awaited"
           (List.print Span.print)
           (Inference.Var.spans var |> SpanSet.to_list)))
;;
