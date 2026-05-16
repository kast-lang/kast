use (import "../common.ks").*;
use (import "../context.ks").*;

module:

const expect_known_type = (
    expected_ty :: Option.t[Ir.Type],
    .span :: Span,
) -> Ir.Type => (
    match expected_ty with (
        | :Some ty => ty
        | :None => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("Unable to infer type");
                ),
                .span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    )
);

const field_ty = (
    obj_ty :: Ir.Type,
    field :: String,
    .field_span :: Span,
) -> Ir.Type => with_return (
    if obj_ty is :ContextObject then (
        return find_context_type(field, .span = field_span);
    );
    let obj_ty = Ir.type_repr(&obj_ty);
    if obj_ty^ is :Named name then (
        let type_def = &(@current Compiler).program.types
            |> OrdMap.get(name)
            |> Option.unwrap;
        let field_ty = match type_def^.shape with (
            | :Union { .variants = ref variants } => (
                variants |> OrdMap.get(field)
            )
            | :Struct { .fields = ref fields } => (
                fields |> OrdMap.get(field)
            )
            | _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        Ir.Print.type_name(obj_ty);
                        (@current Output).write(" doesn't have fields");
                    ),
                    .span = field_span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        );
        match field_ty with (
            | :Some &ty => return ty
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        let output = @current Output;
                        Ir.Print.type_name(obj_ty);
                        output.write(" doesn't have field ");
                        output.write(String.escape(field));
                    ),
                    .span = field_span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );
    let diagnostic = {
        .severity = :Error,
        .source = :Compiler,
        .message = () => (
            Ir.Print.type_name(obj_ty);
            (@current Output).write(" doesn't have fields");
        ),
        .span = field_span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);
