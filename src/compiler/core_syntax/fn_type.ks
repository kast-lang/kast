use (import "./_common.ks").*;

const compile_fn_type = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
) -> TyExpr => (
    let args_ast = (&root^.children |> Tuple.get_named("args"))^
        |> Ast.unwrap_child_value;
    let result = (&root^.children |> Tuple.get_named("result"))^
        |> Ast.unwrap_child_group
        |> AstHelpers.expect_single_child(:None);
    let args_ast = args_ast |> AstHelpers.unwrap_optional("core:scope");
    let mut args = ArrayList.new();
    for arg in Ast.iter_list(
        args_ast,
        .binary_rule_name = "core:comma",
        .trailing_or_leading_rule_name = :Some "core:trailing comma",
    ) do (
        &mut args |> ArrayList.push_back(compile_type_expr(&arg));
    );
    let result = compile_type_expr(&result);
    {
        .shape = :Fn { .args, .result },
        .span = ast^.span,
    }
);

include_ast impl_type_expr_syntax(
    "fn_type",
    compile_fn_type,
)
