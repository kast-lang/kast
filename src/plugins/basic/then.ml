open Std
open Kast_core
open Util

module ExprImpl = struct
  type t = {
    a : Expr.t;
    b : Expr.t;
  }

  type Expr.Shape.t += T of t

  let eval (_span : span) { a; b } (interpreter : Interpreter.t) : Value.t =
    Interpreter.Eval.expr a interpreter |> ignore;
    Interpreter.Eval.expr b interpreter
end

let impl : Compiler.core_syntax =
  {
    name = "then";
    impl =
      (fun (type a)
        (compiler : Compiler.t)
        (kind : a Compilable.t)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let a, b =
          children |> Tuple.map Ast.Child.expect_ast |> Tuple.unwrap_unnamed2
        in
        match kind with
        | Expr ->
            let a = Compiler.compile Expr a compiler in
            let b = Compiler.compile Expr b compiler in
            ExprImpl.T { a; b } |> Compiler.Init.expr span compiler
        | _ ->
            Error.throw span "then must be expr";
            Compilable.error span kind);
  }

let init () =
  Plugin.Expr.register (module ExprImpl);
  Compiler.register_core_syntax impl
