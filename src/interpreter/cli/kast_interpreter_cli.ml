open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
module Interpreter = Kast_interpreter
open Kast_types

module Args = struct
  type args = { path : Uri.t option }
  type t = args

  let parse : string list -> args = function
    | [] -> { path = None }
    | [ path ] -> { path = Some (Uri.file path) }
    | first :: _rest -> fail "Unexpected arg %S" first
end

type evaled = {
  value : value;
  compiler : Compiler.state;
  interpreter : Interpreter.state;
}

let init_compiler_interpreter () =
  let compiler = Compiler.default () in
  (* TODO *)
  let interpreter = compiler.interpreter in
  (compiler, interpreter)

let eval_and : 'a. (evaled option -> 'a) -> Args.t -> 'a =
 fun f { path } ->
  let source = Source.read (path |> Option.value ~default:Uri.stdin) in
  let parsed = Parser.parse source Kast_default_syntax.ruleset in
  match parsed.ast with
  | None -> f None
  | Some ast ->
      let compiler, interpreter = init_compiler_interpreter () in
      let expr : expr = Compiler.compile compiler Expr ast in
      let value : value = Interpreter.eval interpreter expr in
      f (Some { compiler; interpreter; value })

let eval =
  eval_and (function
    | Some { value; _ } -> println "%a" Value.print value
    | None -> println "<none>")

let run = eval_and ignore

let repl (args : Args.t) =
  let evaled =
    match args.path with
    | Some _ -> args |> eval_and (fun result -> result)
    | None -> None
  in
  let compiler, interpreter =
    match evaled with
    | Some { compiler; interpreter; value = _ } -> (compiler, interpreter)
    | None -> init_compiler_interpreter ()
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
        let value : value = Interpreter.eval interpreter expr in
        match value.shape with
        | V_Unit -> ()
        | _ -> println "%a" Value.print value));
    loop ()
  in
  loop ()
