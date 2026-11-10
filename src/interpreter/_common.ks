# TODO this is imported twice because
# macro needs to be compiled outside the module
# but it uses the dependencies
use (import "../span.ks").*;
use (import "../output.ks").*;
use (import "../rc_str.ks").*;
use (import "../diagnostic.ks").*;
use (import "../hir/_lib.ks").*;
use std.rc.Rc;

const impl_value_into = (
    name :: &str,
    T :: std.Type,
    tag :: std.Ast,
) -> std.Ast => @cfg (
    | target.name == "interpreter" => `(
        (value :: Value, .span :: Span) -> T => match value^.shape with (
            | :$tag result => result
            | _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Interpreter,
                    .span,
                    .message = () => (
                        locked_output.write_str("Expected ");
                        locked_output.write_str(name);
                        locked_output.write_str(", got ");
                        Ty.print(&value^.ty);
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
use (import "../rc_str.ks").*;
use std.collections.OrdMap;
use std.rc.Rc;

const expect_value = (
    module:

    const into_int = include_ast impl_value_into("Int", Int, `(Int));
    const into_string = include_ast impl_value_into("String", RcStr, `(String));
    const into_type = include_ast impl_value_into("Type", Ty, `(Type));
    const into_ast = include_ast impl_value_into("Ast", Ast.t, `(Ast));

    const with_span = (
        module:

        const impl_with_span = (name :: std.Ast) -> std.Ast => @cfg (
            | target.name == "interpreter" => `(
                const $name = (args :: ValueWithSpan) => expect_value.$name(...args);
            )
            | true => panic("comptime only pls")
        );

        const into_int = (args :: ValueWithSpan) => expect_value.into_int(...args);
        const into_string = (args :: ValueWithSpan) => expect_value.into_string(...args);
        const into_type = (args :: ValueWithSpan) => expect_value.into_type(...args);
        const into_ast = (args :: ValueWithSpan) => expect_value.into_ast(...args);

        # include_ast impl_with_span(into_int);
        # include_ast impl_with_span(into_string);
        # include_ast impl_with_span(into_type);
        # include_ast impl_with_span(into_ast);
    );
);

const InterpreterContextT = newtype {
    .eval :: &Expr -> Value,
};

const InterpreterContext = @context InterpreterContextT;

const eval = (expr :: &Expr) -> Value => (
    (@current InterpreterContext).eval(expr)
);
