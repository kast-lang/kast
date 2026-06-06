# TODO this is imported twice because
# macro needs to be compiled outside the module
# but it uses the dependencies
use (import "../span.ks").*;
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../hir/_lib.ks").*;

const impl_expect_value = (
    name :: String,
    ty :: std.Type,
    tag :: std.Ast,
) -> std.Ast => @cfg (
    | target.name == "interpreter" => `(
        (value :: Value, .span :: Span) -> ty => match value.shape with (
            | :$(tag) result => result
            | _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Interpreter,
                    .span,
                    .message = () => (
                        let output = @current Output;
                        output.write("Expected ");
                        output.write(name);
                        output.write(", got ");
                        Ty.print(&value.ty);
                    ),
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    )
    | true => panic("runtime only pls")
);

module:

use (import "../id.ks").*;
use (import "../span.ks").*;
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../hir/_lib.ks").*;
use std.collections.OrdMap;

const expect_value = (
    module:

    const expect_int = include_ast impl_expect_value("Int", Int, `(Int));
    const expect_string = include_ast impl_expect_value("String", String, `(String));
    const expect_type = include_ast impl_expect_value("Type", Ty, `(Type));
);
