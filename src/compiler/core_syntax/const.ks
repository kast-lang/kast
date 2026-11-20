use (import "./_common.ks").*;

const compile_const = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let { pattern, value_ast } = root
        |> AstHelpers.expect_two_children(:Some { "pattern", "value" });
    let pattern = compile[Pattern](pattern, .expected_ty = :None);
    let value = compile[Expr](value_ast, .expected_ty = :Some pattern^.ty);
    let value = Interpreter.eval(&value);
    with Interpreter.PatternMatchContext = {
        .matched_binding = (binding, value) => (
            Scope.inject_const(
                binding,
                Interpreter.claim(value, .span = binding^^.span),
            );
        ),
    };
    let value = Place.init(value);
    Interpreter.pattern_match(&pattern, value);
    let expr_shape = if (@current Scope.Context)^.recursive then (
        :Assign {
            .assignee = Rc.new({
                .shape = :Let pattern,
                .span = pattern^.span,
                .ty = pattern^.ty,
            }),
            .value = Rc.new({
                .shape = :Const value,
                .ty = pattern^.ty,
                .span = value_ast^.span,
            }),
        }
    ) else (
        :Unit
    );
    {
        .shape = :Expr expr_shape,
        .ty = Ty.UNIT,
    }
);

include_ast impl_any_expr_syntax(
    "const",
    compile_const,
)
