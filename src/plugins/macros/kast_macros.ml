module AstP = Ast
open Kast_core
open Util

module CompilerState = struct
  type t = { custom_syntax_impls : (Id.t, Value.t) Hashtbl.t }

  let id = Id.gen ()
  let init () = { custom_syntax_impls = Hashtbl.create 0 }
end

let custom_syntax_handler (type a) (span : span) ({ rule; root } : Ast.complex)
    (compiler : Compiler.t) (kind : a Compilable.t) : (Ast.t, unit) Result.t =
  let { custom_syntax_impls } : CompilerState.t =
    compiler.plugins |> Compiler.Plugin.Storage.get (module CompilerState)
  in
  match Hashtbl.find_opt custom_syntax_impls rule.id with
  | Some impl -> (
      (* TODO *)
      let args =
        root.children
        |> Tuple.map Ast.Child.expect_ast
        |> Tuple.map (fun ast : Value.t -> { shape = AstP.Value.T ast })
      in
      let arg : Value.t =
        { shape = Kast_basic_types.Tuple.Value.T { tuple = args } }
      in
      let expr =
        Kast_basic_types.Fn.Expr.Apply.T
          {
            f = Expr.Shape.Const impl |> Compiler.Init.expr span compiler;
            arg = Expr.Shape.Const arg |> Compiler.Init.expr span compiler;
          }
        |> Compiler.Init.expr span compiler
      in
      let result = compiler.interpreter |> Interpreter.Eval.expr expr in
      match result.shape with
      | AstP.Value.T result -> Result.ok result
      | _ ->
          Error.throw span "macro expanded not to ast???";
          Result.error ())
  | None ->
      Error.throw span "Must impl rule before using it: %S" rule.name;
      Result.error ()

let init () =
  AstP.init ();
  Compiler.Plugin.register (module CompilerState);
  Compiler.register_custom_syntax_handler { expand = custom_syntax_handler }
