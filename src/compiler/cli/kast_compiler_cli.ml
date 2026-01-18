open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
module Interpreter = Kast_interpreter
open Kast_types
module Target = Kast_compiler_targets.Target

module Args = struct
  type args =
    { path : Uri.t
    ; target : Target.t
    ; output : string option
    ; formatter : string option
    ; no_std : bool
    }

  type t = args

  let default_target = Target.Ir

  let default path =
    { path; target = default_target; no_std = false; output = None; formatter = None }
  ;;

  let rec parse : string list -> args * rest:string list = function
    | "--no-std" :: rest ->
      let parsed, ~rest = parse rest in
      { parsed with no_std = true }, ~rest
    | "--output" :: path :: rest ->
      let parsed, ~rest = parse rest in
      { parsed with output = Some path }, ~rest
    | "--format" :: formatter :: rest ->
      let parsed, ~rest = parse rest in
      { parsed with formatter = Some formatter }, ~rest
    | "--target" :: target :: rest ->
      let target = Target.parse target in
      let parsed, ~rest = parse rest in
      { parsed with target }, ~rest
    | path :: rest when not (path |> String.starts_with ~prefix:"-") ->
      default (Uri.file path), ~rest
    | rest -> default Uri.stdin, ~rest
  ;;

  let parse_full args =
    let args, ~rest = parse args in
    match rest with
    | [] -> args
    | first :: _rest -> fail "Unexpected arg %S" first
  ;;
end

let run_formatter_if_needed : Args.t -> unit =
  fun { output; formatter; _ } ->
  match output, formatter with
  | Some path, Some "prettier" ->
    let exit_code =
      Sys.command (make_string "prettier --write %s --ignore-path /dev/null" path)
    in
    if exit_code <> 0 then fail "prettier failed with exit code %d" exit_code
  | _ -> ()
;;

let run : Args.t -> unit =
  fun ({ path; target; no_std; output; formatter = _ } as args) ->
  let source = Source.read path in
  let parsed = Parser.parse source Kast_default_syntax.ruleset in
  let out : out_channel =
    match output with
    | None -> stdout
    | Some path ->
      create_dir_all (Filename.dirname path);
      open_out path
  in
  let fmt = Format.formatter_of_out_channel out in
  Format.setup_tty_if_needed fmt out;
  let compiler =
    if no_std
    then (
      let interpreter = Interpreter.default (Uri path) in
      Compiler.init ~import_cache:(Compiler.init_import_cache ()) ~compile_for:interpreter)
    else Compiler.default (Uri source.uri) ()
  in
  let ast = parsed.ast |> Compiler.init_ast in
  let expr : expr = Compiler.compile compiler Expr ast in
  (match target with
   | Ir -> fprintf fmt "%a" Expr.print_with_types expr
   | JavaScript ->
     let transpiled : Kast_transpiler_javascript.result =
       Kast_transpiler_javascript.transpile_expr
         ~state:compiler.interpreter
         ~span:ast.data.span
         expr
     in
     let source_map_path =
       match output with
       | None -> "target/source.map"
       | Some path -> path ^ ".map"
     in
     let writer = Kast_transpiler_javascript.Writer.init fmt source_map_path in
     transpiled.print writer;
     writer |> Kast_transpiler_javascript.Writer.finish);
  close_out out;
  run_formatter_if_needed args
;;
