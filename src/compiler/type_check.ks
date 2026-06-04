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
    let fail = () => (@current TypeCheckContext).fail(
        () => (
            let output = @current Output;
            output.write("Expected ");
            Ty.print(&expected);
            output.write(", got ");
            Ty.print(&actual);
        ),
    );
    match { actual.shape, expected.shape } with (
        | { :Unit, :Unit } => ()
        | { :Unit, _ } => fail()
        | { _, :Unit } => fail()
        | { :Int, :Int } => ()
        | { :Int, _ } => fail()
        | { _, :Int } => fail()
        | { :Type, :Type } => ()
        | { :Type, _ } => fail()
        | { _, :Type } => fail()
    )
);
