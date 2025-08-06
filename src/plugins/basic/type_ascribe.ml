open Std
open Kast_core
open Util

let impl : Compiler.core_syntax =
  {
    name = "type ascribe";
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
        let expr, expected_ty =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named2 [ "expr"; "type" ]
        in
        let expr = compiler |> Compiler.compile kind expr in
        let expr_data = Compilable.Data.get kind expr in
        let expected_ty = compiler |> Compiler.Eval.ty expected_ty in
        expr_data.ty |> Ty.infer_same_as ~span:expr_data.span expected_ty;
        expr);
  }

let init () = Compiler.register_core_syntax impl
