use (import "./_common.ks").*;

const compile_field = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let { obj, field } = root
        |> AstHelpers.expect_two_children(:Some { "obj", "field" });
    let field = field |> AstHelpers.expect_ident;
    let obj = compile[PlaceExpr](obj, .expected_ty = :None);
    let obj_ty = &obj^.ty |> Ty.ensure_inferred_or(() => (
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .span = obj^.span,
            .message = () => (
                locked_output.write_str("Failed to infer obj type");
            ),
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    ));
    let field_doesnt_exist = () => (
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .span = obj^.span,
            .message = () => (
                locked_output.write_str("No field ");
                locked_output.write(&field);
                locked_output.write_str(" in type ");
                Ty.print(&obj^.ty);
            ),
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
    let field_ty = match obj_ty^ with (
        | :Tuple ref tuple => (
            match &tuple^^.fields |> Tuple.get_named_opt(&field) with (
                | :Some &field_ty => field_ty
                | :None => field_doesnt_exist()
            )
        )
        | _ => field_doesnt_exist()
    );
    {
        .shape = :Place :Field {
            .obj,
            .field,
        },
        .ty = field_ty,
    }
);

include_ast impl_any_expr_syntax(
    ".",
    compile_field,
)
