use (import "./_common.ks").*;

const quote = (
    module:

    const Context = @context newtype {
        .depth :: Int32,
        .do_unquote :: Option.t[type (Expr -> ())],
    };

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
        part :: &Ast.InterpolatedStringPart,
    ) -> QuoteExpr.InterpolatedStringPart => (
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
        part :: &Ast.Part,
    ) -> QuoteExpr.Part => (
        match part^ with (
            | :Ignored token => :Ignored token
            | :Keyword token => :Keyword token
            | :Value ref ast => :Value go(ast)
            | :Group ref group => :Group go_group(group)
        )
    );

    const go_group = (
        group :: &Ast.Group,
    ) -> QuoteExpr.Group => (
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
        child :: &Ast.Child,
    ) -> QuoteExpr.Child => (
        match child^ with (
            | :Value ref ast => :Value go(ast)
            | :Group ref group => :Group go_group(group)
        )
    );

    const go = (ast :: &Ast.t) -> QuoteExpr.t => (
        let shape = match ast^.shape with (
            | :Empty => :Construct :Empty
            | :Token token => :Construct :Token token
            | :InterpolatedString {
                .delimiter,
                .open,
                .parts = ref parts,
                .close,
                .stripped_indentation,
            } => :Construct :InterpolatedString {
                .delimiter,
                .open,
                .parts = go_list(parts, go_interpolated_string_part),
                .close,
                .stripped_indentation,
            }
            | :Rule {
                .rule,
                .root = ref root,
            } => with_return (
                if rule.name == "core:quote" then (
                    with Context = {
                        .depth = (@current Context).depth + 1,
                        .do_unquote = (@current Context).do_unquote,
                    };
                    return :Construct :Rule {
                        .rule,
                        .root = go_group(root),
                    };
                );
                if rule.name == "core:unquote" then (
                    with Context = {
                        .depth = (@current Context).depth - 1,
                        .do_unquote = :Some (
                            match (@current Context).do_unquote with (
                                | :None => (expr => return :Unquote expr)
                                | :Some f => f
                            )
                        ),
                    };
                    if (@current Context).depth == 0 then (
                        let unquoted = root^ |> AstHelpers.expect_single_child(:None);
                        let expr = compile(&unquoted, .expected_ty = :Some Ty.AST);
                        ((@current Context).do_unquote |> Option.unwrap)(expr);
                    );
                    return :Construct :Rule {
                        .rule,
                        .root = go_group(root),
                    };
                );
                :Construct :Rule {
                    .rule,
                    .root = go_group(root),
                }
            )
            | :Syntax {
                .command,
                .value_after = ref value_after,
            } => :Construct :Syntax {
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
            } => :Construct :Error {
                .parts = go_list(parts, go_part),
            }
        );
        {
            .shape,
            .ignored_tokens_before = ast^.ignored_tokens_before,
            .span = ast^.span,
        }
    );
);

const compile_quote = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let quoted = root^
        |> AstHelpers.expect_single_child(:None);
    with quote.Context = {
        .depth = 1,
        .do_unquote = :None,
    };
    {
        .shape = :Expr :Quote quote.go(&quoted),
        .ty = Ty.AST,
    }
);

include_ast impl_any_expr_syntax(
    "quote",
    compile_quote,
)
