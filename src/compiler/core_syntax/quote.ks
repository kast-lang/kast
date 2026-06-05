use (import "./_common.ks").*;

const compile_quote = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let quoted = root^
        |> AstHelpers.expect_single_child(:None);
    # TODO
    let quoted = Place.init(
        {
            .shape = :Ast quoted,
            .ty = Ty.AST,
        }
    );
    {
        .shape = :Expr :Const quoted,
        .ty = Ty.AST,
    }
);

include_ast impl_any_expr_syntax(
    "quote",
    compile_quote,
)
