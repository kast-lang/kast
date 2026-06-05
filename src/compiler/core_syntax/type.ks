use (import "./_common.ks").*;

const compile_type = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
) -> TyExpr => (
    {
        .shape = :Expr {
            .shape = :Const Place.init({ .shape = :Type Ty.TYPE, .ty = Ty.TYPE }),
            .ty = Ty.TYPE,
            .span = ast^.span,
        },
        .span = ast^.span,
    }
);

include_ast impl_type_expr_syntax(
    "type",
    compile_type,
)
