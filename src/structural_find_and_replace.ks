# Find this:
#   &group^.parts |> ArrayList.at(part_idx)
# and replace with this:
#   group^.parts.[part_idx]
#
# kast find-ast --pattern '&$arr |> ArrayList.at($idx)' --replace '$arr.[$idx]'
use (import "./tuple.ks").*;
use (import "./span.ks").*;
use (import "./source.ks").*;
use (import "./source_path.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./parser.ks").*;
use (import "./token.ks").*;
use (import "./token_stream.ks").*;
use (import "./lexer/_lib.ks").*;
use (import "./output.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./syntax_ruleset.ks").*;
use (import "./ast.ks").*;
use (import "./highlight.ks").*;
use std.collections.OrdMap;

module:

const StructuralFindAndReplace = (
    module:

    const Pattern = newtype {
        .ast :: Ast.t,
        .bindings :: OrdMap.t[String, type (&Ast.t)],
    };

    const compile_pattern = (
        source :: Source,
        .ruleset :: SyntaxRuleset.t,
    ) -> Pattern => (
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let parsed = Parser.parse(
            .ruleset,
            .entire_source_span = Source.entire_span(&source),
            .path = source.path,
            .token_stream = &mut token_stream,
        );
        let mut bindings = OrdMap.new();
        with FindBindingsContext = {
            .bindings = &mut bindings,
        };
        find_bindings_ast(&parsed.ast);
        {
            .ast = parsed.ast,
            .bindings,
        }
    );

    const FindBindingsContext = @context newtype {
        .bindings :: &mut OrdMap.t[String, type (&Ast.t)],
    };

    const find_bindings_ast = (ast :: &Ast.t) => with_return (
        if unquote_name(ast) is :Some name then (
            (@current FindBindingsContext).bindings |> OrdMap.add(name, ast);
            return;
        );
        match ast^.shape with (
            | :Empty => ()
            | :Token _ => ()
            | :Rule { .root = ref root, ... } => (
                find_bindings_ast_group(root);
            )
            | :Syntax { .value_after, ... } => (
                if value_after is :Some ref ast then (
                    find_bindings_ast(ast);
                );
            )
        );
    );

    const find_bindings_ast_group = (group :: &Ast.Group) => (
        for part in &group^.parts |> ArrayList.iter do (
            match part^ with (
                | :Ignored _ => ()
                | :Keyword _ => ()
                | :Value ref ast => find_bindings_ast(ast)
                | :Group ref group => find_bindings_ast_group(group)
            );
        );
    );

    const Found = (
        module:

        const t = newtype {
            .ast :: &Ast.t,
            .bindings :: OrdMap.t[String, type (&Ast.t)],
        };

        const print_ast = (ast :: &Ast.t) => (
            let output = @current Output;
            Highlight.print_single_line(ast);
            ansi.with_mode(
                :Dim,
                () => (
                    output.write(" at ");
                    Span.print(ast^.span);
                ),
            );
        );

        const print = (self :: &Found.t) => (
            let output = @current Output;
            output.write("Found ");
            print_ast(self^.ast);
            output.write(":\n");
            for &{ .key = name, .value = ast } in &self^.bindings |> OrdMap.iter do (
                output.write(name);
                output.write(" = ");
                print_ast(ast);
                output.write("\n");
            );
        );
    );

    const TryMatchContext = @context newtype {
        .bindings :: &mut OrdMap.t[String, type (&Ast.t)],
        .fail :: () -> Never,
    };

    const try_match = (
        ast :: &Ast.t,
        pattern :: &Pattern,
    ) -> Option.t[Found.t] => with_return (
        let mut bindings = OrdMap.new();
        with TryMatchContext = {
            .bindings = &mut bindings,
            .fail = () => return :None,
        };
        walk_and_match_ast(ast, .pattern = &pattern^.ast);
        :Some { .ast, .bindings }
    );

    const unquote_name = (ast :: &Ast.t) -> Option.t[String] => with_return (
        if ast^.shape is :Rule { .rule, .root } then (
            if rule.name == "core:unquote" then (
                let child = root.children |> Tuple.unwrap_unnamed_1 |> Ast.unwrap_child_value;
                if child.shape is :Token token then (
                    return :Some Token.raw(token);
                );
            );
        );
        :None
    );

    const walk_and_match_ast = (
        ast :: &Ast.t,
        .pattern :: &Ast.t,
    ) => with_return (
        let ctx = @current TryMatchContext;
        if unquote_name(pattern) is :Some name then (
            ctx.bindings |> OrdMap.add(name, ast);
            return;
        );
        match { ast^.shape, pattern^.shape } with (
            | { :Empty, :Empty } => ()
            | { :Token a, :Token b } => (
                if Token.raw(a) != Token.raw(b) then (
                    ctx.fail();
                );
            )
            | {
                :Rule { .rule = ref ast_rule, .root = ref ast_root },
                :Rule { .rule = ref pattern_rule, .root = ref pattern_root },
            } => (
                if not SyntaxRule.is_same(ast_rule, pattern_rule) then (
                    ctx.fail();
                );
                walk_and_match_ast_group(ast_root, .pattern = pattern_root);
            )
            | _ => ctx.fail() |> from_never
        )
    );

    const walk_and_match_ast_group = (
        group :: &Ast.Group,
        .pattern :: &Ast.Group,
    ) => (
        let ctx = @current TryMatchContext;
        let mut group_part_idx = 0;
        let mut pattern_part_idx = 0;
        let next_respected_part = (idx, parts) -> Option.t[type (&Ast.Part)] => with_return (
            while idx^ < parts |> ArrayList.length do (
                let part = parts |> ArrayList.at(idx^);
                idx^ += 1;
                if part^ is :Ignored _ then () else (
                    return :Some part;
                );
            );
            :None
        );
        loop (
            let group_part = next_respected_part(&mut group_part_idx, &group^.parts);
            let pattern_part = next_respected_part(&mut pattern_part_idx, &pattern^.parts);
            match { group_part, pattern_part } with (
                | { :None, :None } => break
                | { :Some a, :Some b } => match { a^, b^ } with (
                    | { :Keyword a, :Keyword b } => (
                        if Token.raw(a) != Token.raw(b) then (
                            ctx.fail();
                        );
                    )
                    | { :Value ref a, :Value ref b } => (
                        walk_and_match_ast(a, .pattern = b);
                    )
                    | { :Group ref a, :Group ref b } => (
                        walk_and_match_ast_group(a, .pattern = b);
                    )
                    | _ => ctx.fail() |> from_never
                )
                | _ => ctx.fail() |> from_never
            )
        );
    );

    const FindContext = @context newtype {
        .pattern :: &Pattern,
        .found :: &mut ArrayList.t[Found.t],
    };

    const find = (ast :: &Ast.t, pattern :: &Pattern) -> ArrayList.t[Found.t] => (
        let mut found = ArrayList.new();
        with FindContext = {
            .pattern,
            .found = &mut found,
        };
        walk_ast(ast);
        found
    );

    const walk_ast = (ast :: &Ast.t) => (
        let ctx = @current FindContext;
        if try_match(ast, ctx.pattern) is :Some found then (
            ctx.found |> ArrayList.push_back(found);
        ) else match ast^.shape with (
            | :Empty => ()
            | :Token _ => ()
            | :InterpolatedString { .parts = ref parts, ... } => (
                for part in parts |> ArrayList.iter do (
                    match part^ with (
                        | :Content _ => ()
                        | :Interpolated { .ast = ref ast, ... } => (
                            walk_ast(ast);
                        )
                    )
                );
            )
            | :Rule { .root = ref root, ... } => walk_ast_group(root)
            | :Syntax { .value_after = ref value_after, ... } => (
                if value_after^ is :Some ref ast then (
                    walk_ast(ast);
                );
            )
        );
    );

    const walk_ast_group = (group :: &Ast.Group) => (
        for { _member, child } in &group^.children |> Tuple.iter do (
            match child^ with (
                | :Value ref ast => walk_ast(ast)
                | :Group ref group => walk_ast_group(group)
            )
        );
    );
);
