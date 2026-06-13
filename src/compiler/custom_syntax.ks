use (import "./_common.ks").*;

module:

const CustomSyntax = newtype {
    .pattern_root :: Ast.Group,
    .bindings :: OrdMap.t[String, Binding],
    .@"impl" :: Expr,
};

impl CustomSyntax as module = (
    module:

    const Context = @context type (&CustomSyntax);

    const inject_ast_bindings = (pattern :: &Ast.t, ast :: &Ast.t) => with_return (
        if pattern^.shape is :Rule { .rule, .root } then (
            if rule.name == "core:unquote" then (
                let name = root
                    |> AstHelpers.expect_single_child(:None)
                    |> AstHelpers.expect_ident;
                let binding = &(@current Context)^.bindings
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                Interpreter.Scope.inject_binding(
                    binding,
                    Place.init(
                        {
                            .shape = :Ast ast^,
                            .ty = Ty.AST,
                        }
                    ),
                );
                return;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Internal,
            .span = pattern^.span,
            .message = () => (
                let output = @current Output;
                output.write("incorrect pattern, should not have compiled");
            ),
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const inject_group_bindings = (pattern_group :: &Ast.Group, group :: &Ast.Group) => (
        for { member, child } in &group^.children |> Tuple.iter do (
            let child_pattern = &pattern_group^.children
                |> Tuple.get(member);
            match { child_pattern^, child^ } with (
                | { :Value ref pattern, :Value ref ast } => inject_ast_bindings(pattern, ast)
                | { :Group ref pattern, :Group ref group } => inject_group_bindings(pattern, group)
            );
        );
    );

    const expand = (self :: &CustomSyntax, root :: &Ast.Group) -> Ast.t => (
        with Context = self;
        with Interpreter.Scope.Context = Interpreter.Scope.new(
            .parent = :Some &(@current Interpreter.Scope.Context),
        );
        inject_group_bindings(&self^.pattern_root, root);
        Interpreter.eval(&self^.@"impl")
            |> Interpreter.expect_value.expect_ast(.span = self^.@"impl".span)
    );
);
