use (import "./_common.ks").*;

module:

const TypeCheckContext = @context newtype {
    .fail :: (() -> ()) -> (),
};

const type_check = (
    .actual :: Ty,
    .expected :: Ty,
    .span :: Span,
) => (
    with TypeCheckContext = {
        .fail = (message) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span,
                .message,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    };
    type_check_impl(.actual, .expected)
);

const type_check_impl = (
    .actual :: Ty,
    .expected :: Ty,
) => (
    let fail = () => (@current TypeCheckContext).fail(() => (
        locked_output.write_str("Expected ");
        Ty.print(&expected);
        locked_output.write_str(", got ");
        Ty.print(&actual);
    ));
    # TODO not unite, but substyping
    let unite_shape = (actual, expected) => (
        match ({ &actual, &expected } :: { _, _ }) with (
            | { &(:Unit), &(:Unit) } => ()
            | { &(:Unit), _ } => fail()
            | { _, &(:Unit) } => fail()
            | { &(:Int), &(:Int) } => ()
            | { &(:Int), _ } => fail()
            | { _, &(:Int) } => fail()
            | { &(:Type), &(:Type) } => ()
            | { &(:Type), _ } => fail()
            | { _, &(:Type) } => fail()
        );
        actual
    );
    Inference.Var.unite(actual, expected, unite_shape);
);
