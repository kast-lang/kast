use (import "./_common.ks").*;

const compile_fn = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let args_ast = (&root^.children |> Tuple.get_named("args"))^
        |> Ast.unwrap_child_value;
    let args_ast = args_ast |> AstHelpers.unwrap_optional("core:scope");
    let mut args = ArrayList.new();
    for arg in Ast.iter_list(
        args_ast,
        .binary_rule_name = "core:comma",
        .trailing_or_leading_rule_name = :Some "core:trailing comma",
    ) do (
        &mut args |> ArrayList.push_back(compile[Pattern](&arg, .expected_ty = :None));
    );
    let result_type = (&root^.children |> Tuple.get_named_opt("result_type"))
        |> Option.map(
            &result_type_ast => (
                let result_type_ast = result_type_ast
                    |> Ast.unwrap_child_group
                    |> AstHelpers.expect_single_child(:None);
                eval_ast_as_type(&result_type_ast)
            )
        );
    let body_ast = (&root^.children |> Tuple.get_named("body"))^
        |> Ast.unwrap_child_value;
    let body = (
        with Scope.Context = Scope.new(.parent = :Some &(@current Scope.Context));
        for arg in &args |> ArrayList.iter do (
            Scope.inject_pattern_bindings(arg);
        );
        compile[Expr](&body_ast, .expected_ty = result_type)
    );
    let ty :: FnTy = {
        .args = &args
            |> ArrayList.iter
            |> std.iter.map(pattern => pattern^.ty)
            |> ArrayList.from_iter,
        .result = body.ty,
    };
    {
        .shape = :Expr :Fn { .args, .body },
        .ty = { .shape = :Fn ty },
    }
);

include_ast impl_any_expr_syntax(
    "fn",
    compile_fn,
)
