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
    | true => panic("comptime only pls")
);

module:

use (import "../id.ks").*;
use (import "../span.ks").*;
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../ast.ks").*;
use (import "../token.ks").*;
use (import "../tuple.ks").*;
use (import "../hir/_lib.ks").*;
use std.collections.OrdMap;

const expect_value = (
    module:

    const expect_int = include_ast impl_expect_value("Int", Int, `(Int));
    const expect_string = include_ast impl_expect_value("String", String, `(String));
    const expect_type = include_ast impl_expect_value("Type", Ty, `(Type));
    const expect_ast = include_ast impl_expect_value("Ast", Ast.t, `(Ast));

    const with_span = (
        module:

        const impl_with_span = (name :: String) -> std.Ast => @cfg (
            | target.name == "interpreter" => `(
                const $name = (args :: ValueWithSpan) => expect_value.$name(...args);
            )
            | true => panic("comptime only pls")
        );

        const expect_int = (args :: ValueWithSpan) => expect_value.expect_int(...args);
        const expect_string = (args :: ValueWithSpan) => expect_value.expect_string(...args);
        const expect_type = (args :: ValueWithSpan) => expect_value.expect_type(...args);
        const expect_ast = (args :: ValueWithSpan) => expect_value.expect_ast(...args);
    # include_ast impl_with_span(expect_int);
    # include_ast impl_with_span(expect_string);
    # include_ast impl_with_span(expect_type);
    # include_ast impl_with_span(expect_ast);
    );
);

const InterpreterContextT = newtype {
    .eval :: &Expr -> Value,
};

const InterpreterContext = @context InterpreterContextT;

const eval = (expr :: &Expr) -> Value => (
    (@current InterpreterContext).eval(expr)
);
