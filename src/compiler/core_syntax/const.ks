use (import "./_common.ks").*;

const compile_const = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let { pattern, value } = root^
        |> AstHelpers.expect_two_children(:Some { "pattern", "value" });
    let pattern = compile[Pattern](&pattern, .expected_ty = :None);
    let value = compile[Expr](&value, .expected_ty = :Some pattern.ty);
    let value = Interpreter.eval(&value);
    with Interpreter.PatternMatchContext = {
        .matched_binding = (binding, value) => (
            Scope.inject_const(
                binding^.name,
                Interpreter.claim(value, .span = pattern.span),
            );
        ),
    };
    Interpreter.pattern_match(&pattern, Place.init(value));
    {
        .shape = :Expr :Unit,
        .ty = Ty.UNIT,
    }
);

include_ast impl_any_expr_syntax(
    "const",
    compile_const,
)
