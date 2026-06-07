use (import "./_common.ks").*;

const compile_mut = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> Pattern => (
    let inner = root^
        |> AstHelpers.expect_single_child(:None);
    with InMut = true;
    compile[Pattern](&inner, .expected_ty)
);

include_ast impl_pattern_syntax(
    "mut",
    compile_mut,
)
