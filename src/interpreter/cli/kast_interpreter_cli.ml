open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
module Interpreter = Kast_interpreter
open Kast_types

module Args = struct
  type args = {
    path : Uri.t option;
    no_std : bool;
  }

  type t = args

  let rec parse : string list -> args = function
    | [] -> { path = None; no_std = false }
    | "--no-std" :: rest -> { (parse rest) with no_std = true }
    | path :: _rest as argv ->
        Kast_interpreter.Natives.Sys.argv := argv |> Array.of_list;
        { path = Some (Uri.file path); no_std = false }
end

type evaled = {
  value : value;
  compiler : Compiler.state;
  interpreter : Interpreter.state;
}

let init_compiler_interpreter ~no_std name_part =
  if no_std then
    let interpreter = Interpreter.default name_part in
    let compiler =
      Compiler.init
        ~import_cache:(Compiler.init_import_cache ())
        ~compile_for:interpreter
    in
    (compiler, interpreter)
  else
    let compiler = Compiler.default name_part () in
    (* TODO *)
    let interpreter = compiler.interpreter in
    (compiler, interpreter)

let eval_and : 'a. (evaled option -> 'a) -> Args.t -> 'a =
 fun f { path; no_std } ->
  let name_part : Types.name_part =
    match path with
    | Some path -> Uri path
    | None -> Str "<stdin>"
  in
  let source = Source.read (path |> Option.value ~default:Uri.stdin) in
  let parsed = Parser.parse source Kast_default_syntax.ruleset in
  match parsed.ast with
  | None -> f None
  | Some ast ->
      let compiler, interpreter = init_compiler_interpreter ~no_std name_part in
      let expr : expr = Compiler.compile compiler Expr ast in
      let value : value = Interpreter.eval interpreter expr in
      f (Some { compiler; interpreter; value })

let eval =
  eval_and (function
    | Some { value; _ } -> println "%a" Value.print value
    | None -> println "<none>")

let run = eval_and ignore

let repl ({ path; no_std } as args : Args.t) =
  let evaled =
    match path with
    | Some _ -> args |> eval_and (fun result -> result)
    | None -> None
  in
  let compiler, interpreter =
    match evaled with
    | Some { compiler; interpreter; value = _ } -> (compiler, interpreter)
    | None -> init_compiler_interpreter ~no_std (Str "<repl>")
  in
  let rec loop () =
    print_string "> ";
    flush stdout;
    let line = input_line stdin in
    let source : source = { contents = line; uri = Uri.stdin } in
    let parsed = Parser.parse source Kast_default_syntax.ruleset in
    (match parsed.ast with
    | None -> ()
    | Some ast -> (
        let expr : expr = Compiler.compile compiler Expr ast in
        try
          let value : value = Interpreter.eval interpreter expr in
          match value.var |> Kast_inference.Var.inferred_opt with
          | Some V_Unit -> ()
          | _ -> println "%a" Value.print value
        with Interpreter.Natives.Panic s -> eprintln "@{<red>panic: %s@}" s));
    loop ()
  in
  loop ()
