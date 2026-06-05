use (import "./_common.ks").*;

const compile_then = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let mut asts = ArrayList.new();
    for expr in Ast.iter_list(
        ast^,
        .binary_rule_name = "core:then",
        .trailing_or_leading_rule_name = :None,
    ) do (
        &mut asts |> ArrayList.push_back(expr);
    );
    let mut list = ArrayList.new();
    let len = &asts |> ArrayList.length;
    for { i, ast } in asts |> ArrayList.into_iter |> std.iter.enumerate do (
        let expected_ty = if i + 1 < len then (
            :None
        ) else (
            expected_ty
        );
        &mut list |> ArrayList.push_back(compile[Expr](&ast, .expected_ty));
    );
    {
        .shape = :Expr :Then list,
        .ty = Ty.UNIT,
    }
);

include_ast impl_any_expr_syntax(
    "then",
    compile_then,
)
