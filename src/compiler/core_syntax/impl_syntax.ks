use (import "./_common.ks").*;

const M = (
    module:

    const Context = @context newtype {
        .bindings :: OrdMap.t[String, Binding],
    };

    const inject_ast_bindings = (ast :: Ast.t) => with_return (
        if ast.shape is :Rule { .rule, .root } then (
            if rule.name == "core:unquote" then (
                let name = root
                    |> AstHelpers.expect_single_child(:None)
                    |> AstHelpers.expect_ident;
                let binding :: Binding = {
                    .id = Id.gen(),
                    .name = name,
                    .ty = Ty.AST,
                    .mutable = false,
                };
                Scope.inject_binding(&binding);
                &mut (@current Context).bindings |> OrdMap.add(name, binding);
                return;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .span = ast.span,
            .message = () => (
                let output = @current Output;
                output.write("Can only use bindings (e.g. `\\name`) as children of impl syntax pattern");
            ),
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
    const inject_group_bindings = (group :: Ast.Group) => (
        for { _member, child } in group.children |> Tuple.into_iter do (
            match child with (
                | :Value ast => inject_ast_bindings(ast)
                | :Group group => inject_group_bindings(group)
            );
        );
    );
);

const compile_impl_syntax = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let { pattern, @"impl" } = root^
        |> AstHelpers.expect_two_children(:Some { "pattern", "impl" });
    with M.Context = {
        .bindings = OrdMap.new(),
    };
    with Scope.Context = Scope.new(.parent = :Some &(@current Scope.Context));
    let { rule, pattern_root } = match pattern.shape with (
        | :Rule { .rule, .root } => (
            M.inject_group_bindings(root);
            { rule, root }
        )
        | _ => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = pattern.span,
                .message = () => (
                    let output = @current Output;
                    output.write("Must impl syntax for some syntax rule");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    );
    let @"impl" = compile[Expr](&@"impl", .expected_ty = :Some Ty.AST);
    (@current CompilerContext).impl_syntax(
        .rule,
        .pattern_root,
        .bindings = (@current M.Context).bindings,
        .@"impl",
    );
    {
        .shape = :Expr :Unit,
        .ty = Ty.UNIT,
    }
);

include_ast impl_any_expr_syntax(
    "impl syntax",
    compile_impl_syntax,
)
