use (import "../ast.ks").*;
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../tuple.ks").*;
use (import "../rc_str.ks").*;

module:

const AstHelpers = (
    module:

    const expect_ident = (ast :: &Ast.t) -> RcStr => with_return (
        if ast^.shape^ is :Token { .shape = ref shape, ... } then (
            if shape^^ is :Ident { .name, ... } then (
                return name;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write_str("Expected an ident");
            ),
            .span = ast^.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const expect_string = (ast :: &Ast.t) -> RcStr => with_return (
        if ast^.shape^ is :Token { .shape = ref shape, ... } then (
            if shape^^ is :String { .contents, ... } then (
                return contents;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write_str("Expected a string token");
            ),
            .span = ast^.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const expect_string_literal = (ast :: &Ast.t) -> RcStr => with_return (
        if ast^.shape^ is :Token { .shape = ref shape, ... } then (
            if shape^^ is :String { .contents, ... } then (
                return contents;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write_str("Expected a string literal");
            ),
            .span = ast^.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const expect_rule = (
        ast :: &Ast.t,
        rule_name :: &str,
    ) -> &Ast.Group => with_return (
        if ast^.shape^ is :Rule { .rule = ref rule, .root = ref root } then (
            if &rule^^.name |> RcStr.as_str == rule_name then (
                return root;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write_str("Expected ");
                locked_output.write_str(rule_name);
            ),
            .span = ast^.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const expect_two_children = (
        group :: &Ast.Group,
        child_names :: Option.t[type { &str, &str }],
    ) -> { &Ast.t, &Ast.t } => (
        with std.PanicHandler = handle_panics(group);
        let { a, b } = match child_names with (
            | :None => (
                &group^.children
                    |> Tuple.unwrap_unnamed_2_ref
            )
            | :Some { a, b } => (
                # TODO no allocate
                let a = RcStr.allocate(a);
                let b = RcStr.allocate(b);
                let a = &group^.children |> Tuple.get_named(&a);
                let b = &group^.children |> Tuple.get_named(&b);
                { a, b }
            )
        );
        { a |> Ast.unwrap_child_value, b |> Ast.unwrap_child_value }
    );

    const expect_three_children = (
        group :: &Ast.Group,
        a :: &str,
        b :: &str,
        c :: &str,
    ) -> { &Ast.t, &Ast.t, &Ast.t } => (
        with std.PanicHandler = handle_panics(group);
        let a = RcStr.allocate(a);
        let b = RcStr.allocate(b);
        let c = RcStr.allocate(c);
        {
            &group^.children |> Tuple.get_named(&a) |> Ast.unwrap_child_value,
            &group^.children |> Tuple.get_named(&b) |> Ast.unwrap_child_value,
            &group^.children |> Tuple.get_named(&c) |> Ast.unwrap_child_value,
        }
    );

    const get_child_ast = (
        group :: &Ast.Group,
        name :: &str,
    ) -> &Ast.t => (
        let name = RcStr.allocate(name);
        match &group^.children |> Tuple.get_named_opt(&name) with (
            | :Some child => child |> Ast.unwrap_child_value
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Internal,
                    .message = () => (
                        locked_output.write_str("Failed to find child named ");
                        locked_output.write_escaped(&name);
                    ),
                    .span = group^.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const expect_single_child = (
        group :: &Ast.Group,
        child_name :: Option.t[&str],
    ) -> &Ast.t => (
        with std.PanicHandler = handle_panics(group);
        let child = match child_name with (
            | :None => (
                &group^.children
                    |> Tuple.unwrap_unnamed_1_ref
            )
            | :Some name => (
                let name = RcStr.allocate(name);
                &group^.children |> Tuple.get_named(&name)
            )
        );
        child |> Ast.unwrap_child_value
    );

    const unwrap_optional = (
        ast :: &Ast.t,
        rule_name :: &str,
    ) -> &Ast.t => with_return (
        if ast^.shape^ is :Rule { .rule = ref rule, .root = ref root } then (
            if &rule^^.name |> RcStr.as_str == rule_name then (
                return root |> expect_single_child(:None);
            );
        );
        ast
    );
    # TODO not catch panics but instead use Tuple.get_*_opt or smth
    const handle_panics = (group :: &Ast.Group) -> std.PanicHandlerT => {
        .handle = @move (message :: &str) -> Never => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = @move () => (
                    locked_output.write_str(message);
                ),
                .span = group^.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
);
