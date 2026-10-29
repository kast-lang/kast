use (import "./output.ks").*;
use (import "./span.ks").*;
use (import "./token.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./tuple.ks").*;
use (import "./rc_str.ks").*;
use std.rc.Rc;

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
                .raw :: RcStr,
                .raw_parts :: ArrayList.t[Token.RawStringPart],
                .contents :: RcStr,
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
                    .delimiter :: RcStr,
                    .open :: Token.t,
                    .parts :: ArrayList.t[InterpolatedStringPart],
                    .close :: Token.t,
                    .stripped_indentation :: RcStr,
                }
                | :Rule {
                    .rule :: Rc.t[SyntaxRule.t],
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
            | :Rule Rc.t[SyntaxRule.t]
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
            | :Value Rc.t[Ast.t]
            | :Group Rc.t[Group]
        );

        const part_span = (self :: &Part) -> Span => match self^ with (
            | :Ignored token => token.span
            | :Keyword token => token.span
            | :Value ref ast => t_span(&ast^^)
            | :Group ref group => group^^.span
        );

        const Child = newtype (
            | :Value Rc.t[Ast.t]
            | :Group Rc.t[Group]
        );

        const unwrap_child_group = (self :: &Child) -> &Ast.Group => match self^ with (
            | :Value _ => panic("expected group ast child, got value")
            | :Group ref group => &group^^
        );

        const unwrap_child_value = (self :: &Child) -> &Ast.t => match self^ with (
            | :Value ref value => &value^^
            | :Group _ => panic("expected value ast child, got group")
        );

        const print_child = (self :: &Child) => (
            match self^ with (
                | :Value ref ast => print(&ast^^)
                | :Group ref group => print_group(&group^^)
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
                    () => write_str("<empty>"),
                )
                | :Token ref token => Token.Shape.print(&token^.shape^)
                | :InterpolatedString {
                    .delimiter = ref delimiter,
                    .open = _,
                    .parts = ref parts,
                    .close = _,
                    .stripped_indentation = _,
                } => (
                    ansi.with_mode(
                        :Green,
                        () => (
                            write(delimiter);
                            write_str("interpolated");
                            write(delimiter);
                        ),
                    );
                    write_str(" {\n");
                    output.inc_indentation();
                    for part in parts |> ArrayList.iter do (
                        match part^ with (
                            | :Content { .contents = ref contents, ... } => (
                                ansi.with_mode(
                                    :Green,
                                    () => write_escaped(contents),
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
                                    () => output.write("\\ "),
                                );
                                print(ast);
                            )
                        );
                        output.write(",\n");
                    );
                    output.dec_indentation();
                    output.write("}");
                )
                | :Rule { .rule = ref rule, .root = ref root } => (
                    ansi.with_mode(
                        :Magenta,
                        () => write(&rule^^.name),
                    );
                    write_str(" ");
                    print_group(root);
                )
                | :Syntax { .command = ref command, .value_after = ref value_after } => (
                    if value_after^ is :Some _ then (
                        write_str("{\n");
                        output.inc_indentation();
                    );
                    ansi.with_mode(
                        :Yellow,
                        () => (
                            write_str("@syntax");
                            match command^.shape with (
                                | :FromScratch => write_str("from_scratch")
                                | :Rule ref rule => write_escaped(&rule^^.name)
                            );
                        ),
                    );
                    if value_after^ is :Some ref value then (
                        print(value);
                        write_str("\n");
                        output.dec_indentation();
                        write_str("}");
                    );
                )
                | :Error { .parts = ref parts } => (
                    ansi.with_mode(
                        :Red,
                        () => write_str("<error>"),
                    );
                    write_str("{\n");
                    output.inc_indentation();
                    for part in parts |> ArrayList.iter do (
                        match part^ with (
                            | :Ignored ref token => (
                                ansi.with_mode(
                                    :Dim,
                                    () => write_str("<ignored> "),
                                );
                                Token.Shape.print_impl(&token^.shape^, .verbose = false);
                            )
                            | :Keyword ref token => (
                                ansi.with_mode(
                                    :Magenta,
                                    () => write_str(Token.raw(token)),
                                );
                            )
                            | :Value ref ast => (
                                print(&ast^^);
                            )
                        );
                        write_str(",\n");
                    );
                    output.dec_indentation();
                    write_str("}");
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
                        .shape :: Rc.t[$Shape.t],
                        .ignored_tokens_before :: Rc.t[ArrayList.t[Token.t]],
                        .span :: Span,
                    }
                ),
                .t_span_def = `(
                    (ast :: &$t) -> Span => ast^.span
                ),
                .print_def = `(
                    (ast :: &$t) => (
                        let output = @current Output;
                        $Shape.print(&ast^.shape^);
                        ansi.with_mode(
                            :Dim,
                            () => (
                                output.write(" at ");
                                Span.print(&ast^.span);
                            ),
                        );
                    )
                ),
            }
        )
    ).*;

    const iter_list = (
        ast :: &Ast.t,
        .binary_rule_name :: &str,
        .trailing_or_leading_rule_name :: Option.t[&str],
    ) -> std.iter.Iterable[type (&Ast.t)] => {
        .iter = @move consumer => (
            let recurse = ast => (
                iter_list(
                    ast,
                    .binary_rule_name,
                    .trailing_or_leading_rule_name,
                ).iter(consumer);
            );
            match ast^.shape^ with (
                | :Empty => ()
                | :Token _ => consumer(ast)
                | :InterpolatedString _ => consumer(ast)
                | :Rule { .rule = ref rule, .root = { .children = ref children, ... } } => with_return (
                    if &rule^^.name |> RcStr.as_str == binary_rule_name then (
                        let lhs = children |> Tuple.get_unnamed(0);
                        let rhs = children |> Tuple.get_unnamed(1);
                        recurse(lhs |> Ast.unwrap_child_value);
                        recurse(rhs |> Ast.unwrap_child_value);
                        return;
                    );
                    if trailing_or_leading_rule_name is :Some name then (
                        if &rule^^.name |> RcStr.as_str == name then (
                            let inner = children |> Tuple.get_unnamed(0);
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
