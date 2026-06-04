use (import "./_common.ks").*;

const compile_let = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> Assignee => (
    let pattern = root^
        |> AstHelpers.expect_single_child(:Some "pattern");
    let pattern = compile[Pattern](&pattern, .expected_ty);
    {
        .shape = :Let pattern,
        .ty = pattern.ty,
        .span = ast^.span,
    }
);

include_ast impl_assignee_syntax(
    "let",
    compile_let,
)
