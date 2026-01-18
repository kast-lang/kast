open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
module Interpreter = Kast_interpreter
open Kast_types
module Target = Kast_compiler_targets.Target

module Args = struct
  type args =
    { compiler : Kast_compiler_cli.Args.t
    ; argv_except_program : string list
    ; enable_source_maps : bool
    }

  type t = args

  let parse : string list -> args =
    fun args ->
    let compiler, ~rest = Kast_compiler_cli.Args.parse args in
    let rec parse_rest args =
      match args with
      | "--enable-source-maps" :: rest ->
        { (parse_rest rest) with enable_source_maps = true }
      | rest -> { compiler; argv_except_program = rest; enable_source_maps = false }
    in
    parse_rest rest
  ;;
end

type evaled =
  { value : value
  ; compiler : Compiler.state
  ; interpreter : Interpreter.state
  }

let init_compiler_interpreter ~no_std name_part =
  if no_std
  then (
    let interpreter = Interpreter.default name_part in
    let compiler =
      Compiler.init ~import_cache:(Compiler.init_import_cache ()) ~compile_for:interpreter
    in
    compiler, interpreter)
  else (
    let compiler = Compiler.default name_part () in
    (* TODO *)
    let interpreter = compiler.interpreter in
    compiler, interpreter)
;;

let setup_interpreter_argv (args : Args.t) =
  Kast_interpreter.Natives.Sys.argv
  := Uri.to_string args.compiler.path :: args.argv_except_program |> Array.of_list
;;

let eval_and : 'a. (evaled option -> 'a) -> Args.t -> 'a =
  fun f
    ({ compiler = { path; no_std; target; output = _; formatter = _ }
     ; argv_except_program = _
     ; enable_source_maps = _
     } as args) ->
  setup_interpreter_argv args;
  if target <> Ir then fail "Unsupported evaluation target";
  let name_part : Types.name_part = Uri path in
  let source = Source.read path in
  let parsed = Parser.parse source Kast_default_syntax.ruleset in
  let compiler, interpreter = init_compiler_interpreter ~no_std name_part in
  let ast = parsed.ast |> Compiler.init_ast in
  let expr : expr = Compiler.compile compiler Expr ast in
  let value : value = Interpreter.eval interpreter expr in
  f (Some { compiler; interpreter; value })
;;

let eval =
  eval_and (function
    | Some { value; _ } -> println "%a" Value.print value
    | None -> println "<none>")
;;

let run ({ compiler; argv_except_program; enable_source_maps } as args : Args.t) =
  match compiler.target with
  | Ir -> eval_and ignore args
  | JavaScript ->
    let path =
      match compiler.output with
      | Some path -> path
      | None -> "target/compiled.mjs"
    in
    Kast_compiler_cli.run { args.compiler with output = Some path };
    let node_args = if enable_source_maps then [ "--enable-source-maps" ] else [] in
    let node_args = node_args @ (path :: argv_except_program) in
    Log.info (fun log ->
      log "Launching node with args %a" (List.print String.print_maybe_escaped) node_args);
    Unix.execvp "node" (Array.of_list ("node" :: node_args))
;;

let repl
      ({ compiler = { path; no_std; target = _; output = _; formatter = _ }
       ; argv_except_program = _
       ; enable_source_maps = _
       } as args :
        Args.t)
  =
  let evaled =
    if path = Uri.stdin then None else args |> eval_and (fun result -> result)
  in
  setup_interpreter_argv args;
  let compiler, interpreter =
    match evaled with
    | Some { compiler; interpreter; value = _ } -> compiler, interpreter
    | None -> init_compiler_interpreter ~no_std (Str "<repl>")
  in
  let rec loop () =
    print_string "> ";
    flush stdout;
    let line = input_line stdin in
    let source : source = { contents = line; uri = Uri.stdin } in
    let parsed = Parser.parse source Kast_default_syntax.ruleset in
    let ast = parsed.ast |> Compiler.init_ast in
    let expr : expr = Compiler.compile compiler Expr ast in
    (try
       let value : value = Interpreter.eval interpreter expr in
       value.var |> Kast_inference_base.Var.setup_default_if_needed;
       value |> Kast_inference_completion.complete_value;
       match value.var |> Kast_inference.Var.inferred_opt with
       | Some V_Unit -> ()
       | _ ->
         println "%a @{<italic>:: %a@}" Value.print value Ty.print (Value.ty_of value)
     with
     | Interpreter.Natives.Panic s -> eprintln "@{<red>panic: %s@}" s);
    loop ()
  in
  loop ()
;;
