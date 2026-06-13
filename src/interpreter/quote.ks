use (import "./_common.ks").*;

const quote_impl = (
    # TODO copypasta from quote Ast -> QuoteExpr
    module:

    const go_list = [T, U] (list :: &ArrayList.t[T], f :: &T -> U) -> ArrayList.t[U] => (
        let mut result = ArrayList.new();
        for item in list |> ArrayList.iter do (
            &mut result |> ArrayList.push_back(f(item));
        );
        result
    );

    const go_tuple = [T, U] (tuple :: &Tuple.t[T], f :: &T -> U) -> Tuple.t[U] => (
        let mut result = Tuple.new();
        for { member, value } in tuple |> Tuple.iter do (
            let name = match member with (
                | :Index _ => :None
                | :Name name => :Some name
            );
            &mut result |> Tuple.add(name, f(value));
        );
        result
    );

    const go_interpolated_string_part = (
        part :: &QuoteExpr.InterpolatedStringPart,
    ) -> Ast.InterpolatedStringPart => (
        match part^ with (
            | :Content content => :Content content
            | :Interpolated {
                .open,
                .ast = ref ast,
                .ignored_trailing_tokens,
                .close,
            } => :Interpolated {
                .open,
                .ast = go(ast),
                .ignored_trailing_tokens,
                .close,
            }
        )
    );

    const go_part = (
        part :: &QuoteExpr.Part,
    ) -> Ast.Part => (
        match part^ with (
            | :Ignored token => :Ignored token
            | :Keyword token => :Keyword token
            | :Value ref ast => :Value go(ast)
            | :Group ref group => :Group go_group(group)
        )
    );

    const go_group = (
        group :: &QuoteExpr.Group,
    ) -> Ast.Group => (
        let {
            .parts = ref parts,
            .children = ref children,
            .span,
        } = group^;
        {
            .parts = go_list(parts, go_part),
            .children = go_tuple(children, go_child),
            .span,
        }
    );

    const go_child = (
        child :: &QuoteExpr.Child,
    ) -> Ast.Child => (
        match child^ with (
            | :Value ref ast => :Value go(ast)
            | :Group ref group => :Group go_group(group)
        )
    );

    const go = (expr :: &QuoteExpr.t) -> Ast.t => with_return (
        let shape = match expr^.shape with (
            | :Unquote ref expr => (
                return eval(expr) |> expect_value.expect_ast(.span = expr^.span)
            )
            | :Construct ref shape => match shape^ with (
                | :Empty => :Empty
                | :Token token => :Token token
                | :InterpolatedString {
                    .delimiter,
                    .open,
                    .parts = ref parts,
                    .close,
                    .stripped_indentation,
                } => :InterpolatedString {
                    .delimiter,
                    .open,
                    .parts = go_list(parts, go_interpolated_string_part),
                    .close,
                    .stripped_indentation,
                }
                | :Rule {
                    .rule,
                    .root = ref root,
                } => :Rule {
                    .rule,
                    .root = go_group(root),
                }
                | :Syntax {
                    .command,
                    .value_after = ref value_after,
                } => :Syntax {
                    .command = {
                        .shape = match command.shape with (
                            | :FromScratch => :FromScratch
                            | :Rule rule => :Rule rule
                        ),
                        .raw_tokens = command.raw_tokens,
                    },
                    .value_after = value_after
                        |> Option.as_ref
                        |> Option.map(go),
                }
                | :Error {
                    .parts = ref parts,
                } => :Error {
                    .parts = go_list(parts, go_part),
                }
            )
        );
        {
            .shape,
            .ignored_tokens_before = expr^.ignored_tokens_before,
            .span = expr^.span,
        }
    );
);

module:

const quote = (expr :: &QuoteExpr.t) -> Ast.t => (
    quote_impl.go(expr)
);
