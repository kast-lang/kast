use (import "../ast.ks").*;
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../tuple.ks").*;

module:

const AstHelpers = (
    module:

    const expect_ident = (ast :: Ast.t) -> String => (
        match ast.shape with (
            | :Token { .shape = :Ident { .name, ... }, ... } => name
            | _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        (@current Output).write("Expected an ident");
                    ),
                    .span = ast.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const expect_string = (ast :: Ast.t) -> String => (
        match ast.shape with (
            | :Token { .shape = :String { .contents, ... }, ... } => contents
            | _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        (@current Output).write("Expected a string token");
                    ),
                    .span = ast.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const expect_string_literal = (ast :: Ast.t) -> String => (
        match ast.shape with (
            | :Token { .shape = :String { .contents, ... }, ... } => contents
            | _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        (@current Output).write("Expected a string literal");
                    ),
                    .span = ast.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const expect_rule = (
        ast :: Ast.t,
        rule_name :: String,
    ) -> Ast.Group => with_return (
        if ast.shape is :Rule { .rule, .root } then (
            if rule.name == rule_name then (
                return root;
            )
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                (@current Output).write("Expected " + rule_name);
            ),
            .span = ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const expect_two_children = (
        group :: Ast.Group,
        child_names :: Option.t[type { String, String }],
    ) -> { Ast.t, Ast.t } => (
        with std.PanicHandler = handle_panics(&group);
        let { a, b } = match child_names with (
            | :None => (
                group.children
                    |> Tuple.unwrap_unnamed_2
            )
            | :Some { a, b } => (
                let a = (&group.children |> Tuple.get_named(a))^;
                let b = (&group.children |> Tuple.get_named(b))^;
                { a, b }
            )
        );
        { a |> Ast.unwrap_child_value, b |> Ast.unwrap_child_value }
    );

    const expect_three_children = (
        group :: Ast.Group,
        a :: String,
        b :: String,
        c :: String,
    ) -> { Ast.t, Ast.t, Ast.t } => (
        with std.PanicHandler = handle_panics(&group);
        let a = (&group.children |> Tuple.get_named(a))^;
        let b = (&group.children |> Tuple.get_named(b))^;
        let c = (&group.children |> Tuple.get_named(c))^;
        {
            a |> Ast.unwrap_child_value,
            b |> Ast.unwrap_child_value,
            c |> Ast.unwrap_child_value,
        }
    );

    const get_child_ast = (
        group :: Ast.Group,
        name :: String,
    ) -> Ast.t => (
        match &group.children |> Tuple.get_named_opt(name) with (
            | :Some &child => child |> Ast.unwrap_child_value
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Internal,
                    .message = () => (
                        (@current Output).write("Failed to find child named " + String.escape(name));
                    ),
                    .span = group.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const expect_single_child = (
        group :: Ast.Group,
        child_name :: Option.t[String],
    ) -> Ast.t => (
        with std.PanicHandler = handle_panics(&group);
        let child = match child_name with (
            | :None => (
                group.children
                    |> Tuple.unwrap_unnamed_1
            )
            | :Some name => (&group.children |> Tuple.get_named(name))^
        );
        child |> Ast.unwrap_child_value
    );
    # TODO not catch panics but instead use Tuple.get_*_opt or smth
    const handle_panics = (group :: &Ast.Group) -> std.PanicHandlerT => {
        .handle = [T] (message :: String) -> T => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    (@current Output).write(message);
                ),
                .span = group^.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
);
