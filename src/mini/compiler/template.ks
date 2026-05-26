use (import "./common.ks").*;
use (import "./scope.ks").*;

module:

const TemplateArgsContext = @context Ir.TemplateArgs;

const parse_template = (name :: String, root :: Ast.Group) -> Template => (
    let { args, def } = root
        |> AstHelpers.expect_two_children(:Some { "args", "def" });
    let mut arg_names = ArrayList.new();
    for arg in Ast.iter_list(
        args,
        .binary_rule_name = "comma",
        .trailing_or_leading_rule_name = :Some "trailing comma",
    ) do (
        let arg = arg |> AstHelpers.expect_ident;
        &mut arg_names |> ArrayList.push_back(arg);
    );
    {
        .captured_scope = @current ScopeContext,
        .arg_names,
        .def,
        .instantiations = OrdMap.new_with_compare(compare_template_args),
    }
);

const instantiation_to_type = (
    instantiation :: Instantiation,
    .span :: Span,
) -> Ty => (
    Value.expect_type(instantiation.result, .span)
);

const instantiation_to_expr = (
    instantiation :: Ir.Instantiation,
    .span :: Span,
) -> ParsedExpr => (
    {
        .shape = :Expr :Const instantiation.result,
        .ty = Value.ty(&instantiation.result),
    }
);

const instantiate_ty = (
    template :: &mut Ir.Template,
    args :: ArrayList.t[Ir.Type],
    .span :: Span,
) -> Ir.Type => (
    instantiate(template, args, .span)
        |> instantiation_to_type(.span)
);

const instantiate = (
    template :: &mut Ir.Template,
    args :: ArrayList.t[Ir.Type],
    .span :: Span,
) -> Ir.Instantiation => (
    let expected_args_len = &template^.arg_names |> ArrayList.length;
    Log.debug_msg("calculating template args");
    let args :: Ir.TemplateArgs = (
        let mut by_name = OrdMap.new();
        let args_len = &args |> ArrayList.length;
        let expected_args_len = &template^.arg_names |> ArrayList.length;
        if args_len != expected_args_len then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("Template expected ");
                    output.write(to_string(expected_args_len));
                    output.write(" arg, got ");
                    output.write(to_string(args_len));
                ),
                .span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic);
        );
        for { i, &arg } in &args |> ArrayList.iter |> std.iter.enumerate do (
            let arg_name = &template^.arg_names |> ArrayList.at(i);
            &mut by_name |> OrdMap.add(arg_name^, arg);
        );
        { .args, .by_name }
    );
    Log.debug_msg("calculated template args");
    let do_instantiate = () -> Instantiation => with_return (
        let mut locals = OrdMap.new();
        for &{ .key = name, .value = ty } in &args.by_name |> OrdMap.iter do (
            &mut locals |> OrdMap.add(name, :Const Ir.Value.new_type(ty));
        );
        with ScopeContext = {
            .parent = :Some template^.captured_scope,
            .locals,
            .found_in_parent = (...) => (),
        };
        let result = eval_ast(:None, template^.def);
        { .template = &template^, .template_args = args.args, .result }
    );
    let instantiation = &mut template^.instantiations
        |> OrdMap.get_or_init(args, do_instantiate);
    instantiation^
);

const parse_instantiate = (root :: Ast.Group) -> Ir.Instantiation => (
    let { template, args_ast } = root
        |> AstHelpers.expect_two_children(:Some { "template", "args" });
    let mut template = eval_ast(:None, template)
        |> Ir.Value.expect_template(.span = template.span);

    let mut args = ArrayList.new();
    for arg in Ast.iter_list(
        args_ast,
        .binary_rule_name = "comma",
        .trailing_or_leading_rule_name = :Some "trailing comma",
    ) do (
        &mut args |> ArrayList.push_back((@current Compiler).parse_type(arg));
    );
    instantiate(&mut template, args, .span = root.span)
);

const parse_instantiate_ty = (root :: Ast.Group) -> Ir.Type => (
    parse_instantiate(root)
        |> instantiation_to_type(.span = root.span)
);
