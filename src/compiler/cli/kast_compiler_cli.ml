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
    ; save_dependency_graph : string option
    ; continuous : bool
    }

  type t = args

  let default_target = Target.Ir

  let default path =
    { path
    ; target = default_target
    ; no_std = false
    ; output = None
    ; formatter = None
    ; save_dependency_graph = None
    ; continuous = false
    }
  ;;

  let rec parse : string list -> args * rest:string list = function
    | "--no-std" :: rest ->
      let parsed, ~rest = parse rest in
      { parsed with no_std = true }, ~rest
    | "--continuous" :: rest ->
      let parsed, ~rest = parse rest in
      { parsed with continuous = true }, ~rest
    | "--output" :: path :: rest ->
      let parsed, ~rest = parse rest in
      { parsed with output = Some path }, ~rest
    | "--save-dependency-graph" :: path :: rest ->
      let parsed, ~rest = parse rest in
      { parsed with save_dependency_graph = Some path }, ~rest
    | "--format" :: formatter :: rest ->
      let parsed, ~rest = parse rest in
      { parsed with formatter = Some formatter }, ~rest
    | "--target" :: target :: rest ->
      let target = Target.parse target in
      let parsed, ~rest = parse rest in
      { parsed with target }, ~rest
    | "--use-numbers-instead-of-symbols" :: value :: rest ->
      Kast_transpiler_javascript.use_numbers_instead_of_symbols := bool_of_string value;
      parse rest
    | "--js-ref-vars" :: value :: rest ->
      Kast_transpiler_javascript.ref_vars_enabled := bool_of_string value;
      parse rest
    | "--async" :: value :: rest ->
      (Kast_transpiler_javascript.async_fns
       := match value with
          | "never" -> Kast_transpiler_javascript.Never
          | "always" -> Always
          | "inference" -> BasedOnInference
          | _ -> fail "incorrect async mode");
      parse rest
    | path :: rest when not (path |> String.starts_with ~prefix:"-") ->
      default (Uri.file path), ~rest
    | rest -> default Uri.stdin, ~rest
  ;;

  let parse_full args =
    let args, ~rest = parse args in
    match rest with
    | [] -> args
    | first :: _rest -> fail "Unexpected arg %a" String.print_debug first
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
  fun ({ path; target; no_std; output; save_dependency_graph; continuous; formatter = _ }
       as args) ->
  let cache = Compiler.Cache.init () in
  let do_compile () =
    let source = Source.read path in
    let compiler = Compiler.default ~no_std ~cache (Uri source.uri) () in
    let parsed =
      compiler
      |> Compiler.handle_parser_imports (fun () ->
        Parser.parse source Kast_default_syntax.ruleset)
    in
    let out : out_channel =
      match output with
      | None -> stdout
      | Some path ->
        create_dir_all (Filename.dirname path);
        open_out path
    in
    let fmt = Format.formatter_of_out_channel out in
    Format.setup_tty_if_needed fmt out;
    let ast = parsed.ast |> Kast_ast_init.init_ast in
    let expr : expr = Compiler.compile ~prelude:(not no_std) compiler Expr ast in
    (match target with
     | Ir -> fprintf fmt "%a" Expr.print_with_types expr
     | Minikast minitarget ->
       (match minitarget with
        | JavaScript ->
          let transpiled =
            Kast_transpiler_minikast.transpile_expr
              { name = "javascript" }
              compiler.interpreter
              expr
          in
          Kast_transpiler_minikast.MiniAst.Print.print_program transpiled
        | _ -> fail "not supported minitarget")
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
    run_formatter_if_needed args;
    match save_dependency_graph with
    | None -> ()
    | Some path ->
      let out = open_out path in
      let fmt = Format.formatter_of_out_channel out in
      fprintf fmt "digraph {\n";
      fprintf fmt [%include_file "dep_graph_styling.dot"];
      compiler.cache.dependents
      |> UriMap.iter (fun dependency dependents ->
        dependents
        |> List.iter (fun dependent ->
          fprintf fmt "  %S -> %S;\n" (Uri.to_string dependent) (Uri.to_string dependency)));
      fprintf fmt "}\n";
      close_out out
  in
  let do_compile () =
    try do_compile () with
    | Cancel -> raise Cancel
    | e when continuous ->
      Log.error (fun log -> log "compilation failed");
      Printexc.default_uncaught_exception_handler e (Printexc.get_raw_backtrace ())
  in
  let file_mod_times_when_compiled = ref UriMap.empty in
  let do_continue = ref true in
  while !do_continue do
    let file_mod_time (uri : Uri.t) =
      let path =
        match Uri.scheme uri with
        | Some "file" -> Uri.file_path uri
        | scheme ->
          fail "unsupported uri scheme %a" (Option.print String.print_debug) scheme
      in
      let result = (Unix.stat path).st_mtime in
      Log.trace (fun log -> log "mod time of %a = %f" Uri.print uri result);
      result
    in
    let file_was_changed (uri : Uri.t) : bool =
      match !file_mod_times_when_compiled |> UriMap.find_opt uri with
      | None -> true
      | Some time_when_compiled ->
        let last_mod_time = file_mod_time uri in
        last_mod_time > time_when_compiled
    in
    (try timed "Compilation" do_compile with
     | effect Source.Read uri, k ->
       file_mod_times_when_compiled
       := !file_mod_times_when_compiled |> UriMap.add uri (file_mod_time uri);
       Effect.continue k (Effect.perform (Source.Read uri)));
    if continuous
    then (
      print_string "Press enter to recompile";
      ignore <| read_line ();
      cache.imported
      |> UriMap.iter (fun uri _ ->
        if file_was_changed uri
        then (
          Log.info (fun log -> log "%a was changed since last compilation" Uri.print uri);
          cache |> Compiler.Cache.invalidate uri));
      Log.info (fun log -> log "Recompiling..."))
    else do_continue := false
  done
;;
