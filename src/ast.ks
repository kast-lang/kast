use (import "./output.ks").*;
use (import "./span.ks").*;
use (import "./token.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./tuple.ks").*;

module:

const MakeAstModule = (
    make_t :: (
        .t :: std.Ast,
        .t_span :: std.Ast,
        .Shape :: std.Ast,
    ) -> {
        .t_def :: std.Ast,
        .t_span_def :: std.Ast,
        .print_def :: std.Ast,
    },
) -> std.Ast => (
    `(
        module:

        const Ast = @current_scope;

        const {
            .t_def,
            .t_span_def,
            .print_def,
        } = make_t(
            .t = `(t),
            .Shape = `(Shape),
            .t_span = `(t_span),
        );
        const t = include_ast t_def;
        const t_span = include_ast t_span_def;
        const print = include_ast print_def;

        const InterpolatedStringPart = newtype (
            | :Content {
                .raw :: String,
                .raw_parts :: ArrayList.t[Token.RawStringPart],
                .contents :: String,
                .span :: Span,
            }
            | :Interpolated {
                .open :: Token.t,
                .ast :: Ast.t,
                .ignored_trailing_tokens :: ArrayList.t[Token.t],
                .close :: Token.t,
            }
        );

        const Shape = (
            module:

            const t = newtype (
                | :Empty
                | :Token Token.t
                | :InterpolatedString {
                    .delimiter :: String,
                    .open :: Token.t,
                    .parts :: ArrayList.t[InterpolatedStringPart],
                    .close :: Token.t,
                    .stripped_indentation :: String,
                }
                | :Rule {
                    .rule :: SyntaxRule.t,
                    .root :: Group,
                }
                | :Syntax {
                    .command :: SyntaxCommand,
                    .value_after :: Option.t[Ast.t],
                }
                | :Error {
                    .parts :: ArrayList.t[Part],
                }
            );

            const print = (self :: &t) => Ast.print_shape(self);
        );

        const SyntaxCommandShape = newtype (
            | :FromScratch
            | :Rule SyntaxRule.t
        );

        const SyntaxCommand = newtype {
            .shape :: SyntaxCommandShape,
            .raw_tokens :: ArrayList.t[Token.t],
        };

        const Group = newtype {
            .parts :: ArrayList.t[Part],
            .children :: Tuple.t[Child],
            .span :: Span,
        };

        const Part = newtype (
            | :Ignored Token.t
            | :Keyword Token.t
            | :Value Ast.t
            | :Group Group
        );

        const part_span = (self :: &Part) -> Span => match self^ with (
            | :Ignored token => token.span
            | :Keyword token => token.span
            | :Value ref ast => t_span(ast)
            | :Group group => group.span
        );

        const Child = newtype (
            | :Value Ast.t
            | :Group Group
        );

        const unwrap_child_group = (self :: Child) -> Ast.Group => match self with (
            | :Value _ => panic("expected group ast child, got value")
            | :Group group => group
        );

        const unwrap_child_value = (self :: Child) -> Ast.t => match self with (
            | :Value value => value
            | :Group _ => panic("expected value ast child, got group")
        );

        const print_child = (self :: &Child) => (
            match self^ with (
                | :Value ref ast => print(ast)
                | :Group ref group => print_group(group)
            );
        );

        const print_group = (self :: &Group) => (
            print_children(&self^.children);
        );

        const print_children = (children :: &Tuple.t[Child]) => (
            Tuple.print(
                children,
                .open = "{",
                .delimiter = ",",
                .before_field_name = ".",
                .after_field_name = " = ",
                .print_value = print_child,
                .close = "}",
            );
        );

        const print_shape = (self :: &Ast.Shape.t) => (
            let output = @current Output;
            match self^ with (
                | :Empty => ansi.with_mode(
                    :Dim,
                    () => output.write("<empty>"),
                )
                | :Token token => Token.Shape.print(token.shape)
                | :InterpolatedString {
                    .delimiter,
                    .open = _,
                    .parts = ref parts,
                    .close = _,
                    .stripped_indentation = _,
                } => (
                    ansi.with_mode(
                        :Green,
                        () => (
                            output.write(delimiter);
                            output.write("interpolated");
                            output.write(delimiter);
                        ),
                    );
                    output.write(" {\n");
                    output.inc_indentation();
                    for part in parts |> ArrayList.iter do (
                        match part^ with (
                            | :Content { .contents, ... } => (
                                ansi.with_mode(
                                    :Green,
                                    (


                                    ) => output.write(String.escape(contents)),
                                );
                            )
                            | :Interpolated {
                                .open = _,
                                .close = _,
                                .ast = ref ast,
                                .ignored_trailing_tokens = _,
                            } => (
                                ansi.with_mode(
                                    :Yellow,
                                    (


                                    ) => output.write("\\ "),
                                );
                                print(ast);
                            )
                        );
                        output.write(",\n");
                    );
                    output.dec_indentation();
                    output.write("}");
                )
                | :Rule { .rule, .root = ref root } => (
                    ansi.with_mode(
                        :Magenta,
                        () => output.write(rule.name),
                    );
                    output.write(" ");
                    print_group(root);
                )
                | :Syntax { .command, .value_after } => (
                    if value_after is :Some _ then (
                        output.write("{\n");
                        output.inc_indentation();
                    );
                    ansi.with_mode(
                        :Yellow,
                        () => (
                            output.write("@syntax");
                            match command.shape with (
                                | :FromScratch => output.write("from_scratch")
                                | :Rule rule => output.write(String.escape(rule.name))
                            );
                        ),
                    );
                    if value_after is :Some value then (
                        print(&value);
                        output.write("\n");
                        output.dec_indentation();
                        output.write("}");
                    );
                )
                | :Error { .parts = ref parts } => (
                    ansi.with_mode(
                        :Red,
                        () => output.write("<error>"),
                    );
                    output.write("{\n");
                    output.inc_indentation();
                    for part in parts |> ArrayList.iter do (
                        match part^ with (
                            | :Ignored token => (
                                ansi.with_mode(
                                    :Dim,
                                    (


                                    ) => output.write("<ignored> "),
                                );
                                Token.Shape.print_impl(token.shape, .verbose = false);
                            )
                            | :Keyword token => (
                                ansi.with_mode(
                                    :Magenta,
                                    (


                                    ) => output.write(Token.raw(token)),
                                );
                            )
                            | :Value ref ast => (
                                print(ast);
                            )
                        );
                        output.write(",\n");
                    );
                    output.dec_indentation();
                    output.write("}");
                )
            );
        );

    )
);

const Ast = (
    module:

    use (
        include_ast MakeAstModule(
            (.t, .t_span, .Shape) => {
                .t_def = `(
                    newtype {
                        .shape :: $Shape.t,
                        .ignored_tokens_before :: ArrayList.t[Token.t],
                        .span :: Span,
                    }
                ),
                .t_span_def = `(
                    (ast :: &$t) -> Span => ast^.span
                ),
                .print_def = `(
                    (ast :: &$t) => (
                        let output = @current Output;
                        $Shape.print(&ast^.shape);
                        ansi.with_mode(
                            :Dim,
                            () => (
                                output.write(" at ");
                                Span.print(ast^.span);
                            ),
                        );
                    )
                ),
            }
        )
    ).*;

    const iter_list = (
        ast :: Ast.t,
        .binary_rule_name :: String,
        .trailing_or_leading_rule_name :: Option.t[String],
    ) -> std.iter.Iterable[Ast.t] => {
        .iter = consumer => (
            let recurse = ast => (
                iter_list(
                    ast,
                    .binary_rule_name,
                    .trailing_or_leading_rule_name,
                ).iter(consumer);
            );
            match ast.shape with (
                | :Empty => ()
                | :Token _ => consumer(ast)
                | :InterpolatedString _ => consumer(ast)
                | :Rule { .rule, .root = { .children, ... } } => with_return (
                    if rule.name == binary_rule_name then (
                        let { lhs, rhs } = Tuple.unwrap_unnamed_2(children);
                        recurse(lhs |> Ast.unwrap_child_value);
                        recurse(rhs |> Ast.unwrap_child_value);
                        return;
                    );
                    if trailing_or_leading_rule_name is :Some name then (
                        if rule.name == name then (
                            let inner = Tuple.unwrap_unnamed_1(children);
                            recurse(inner |> Ast.unwrap_child_value);
                            return;
                        );
                    );
                    consumer(ast);
                )
            );
        ),
    };
);
