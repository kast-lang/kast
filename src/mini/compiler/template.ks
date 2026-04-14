use (import "./common.ks").*;

module:

const TemplateArgsContext = @context TemplateArgs;

const compare_template_args = (
    a :: TemplateArgs,
    b :: TemplateArgs,
) -> std.cmp.Ordering => with_return (
    let len_a = &a.args |> ArrayList.length;
    let len_b = &b.args |> ArrayList.length;
    let len_ord = std.cmp.default_compare(len_a, len_b);
    if len_ord is :Equal then () else (
        return len_ord;
    );
    for i in 0..len_a do (
        let ty_a = &a.args |> ArrayList.at(i);
        let ty_b = &b.args |> ArrayList.at(i);
        let ty_ord = Ir.compare_type(ty_a, ty_b);
        if ty_ord is :Equal then () else (
            return ty_ord;
        );
    );
    :Equal
);

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
        .arg_names,
        .def,
        .instantiations = OrdMap.new_with_compare(compare_template_args),
    }
);

const instantiation_to_type = (
    instantiation :: Instantiation,
    .span :: Span,
) -> Ty => (
    let ty = (@current Compiler).lookup_type(instantiation.name, .span);
    if instantiation.template_name == "UnwindToken" then (
        :UnwindToken {
            .instantiated_token_ty = ty,
            .result_ty = ArrayList.at(&instantiation.template_args, 0)^,
        }
    ) else (
        ty
    )
);

const instantiation_to_expr = (
    instantiation :: Instantiation,
    .span :: Span,
) -> ParsedExpr => (
    {
        .shape = :Place :Ident instantiation.name,
        .ty = (@current Compiler).find_ident_ty(instantiation.name, .span),
    }
);

const instantiate_ty = (
    template_name :: String,
    args :: ArrayList.t[Ir.Type],
    .span :: Span,
) -> Ir.Type => (
    instantiate(template_name, args, .span)
        |> instantiation_to_type(.span)
);

const find_template = (
    name :: String,
    .span :: Span,
) -> &mut Template => with_return (
    if (@current Compiler).get_toplevel_impl(name) is :Some item then (
        if item is :Template mut template then (
            # TODO references are messed up
            return &mut template;
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                let output = @current Output;
                output.write(String.escape(name));
                output.write(" is not a template");
            ),
            .span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
    let diagnostic = {
        .severity = :Error,
        .source = :Compiler,
        .message = () => (
            let output = @current Output;
            output.write("Could not find template named ");
            output.write(String.escape(name));
        ),
        .span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);

const instantiate = (
    template_name :: String,
    args :: ArrayList.t[Ir.Type],
    .span :: Span,
) -> Instantiation => (
    let template = find_template(template_name, .span);
    let expected_args_len = &template^.arg_names |> ArrayList.length;
    Log.debug_msg("calculating template args");
    let args :: TemplateArgs = (
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
        let name = output_to_string(
            () => (
                let output = @current Output;
                output.write(template_name);
                for arg in &args.args |> ArrayList.iter do (
                    output.write("_");
                    Ir.Print.type_name_as_ident(arg);
                );
            )
        );
        Log.debug_msg("instantiating " + name);
        let toplevel_item = {
            .name,
            .span,
            .ast = template^.def,
            .native = false,
            .setup_contexts = :Some (
                f => (
                    with TemplateArgsContext = args;
                    f()
                )
            ),
        };
        (@current Compiler).add_toplevel_item(toplevel_item);
        { .template_name, .template_args = args.args, .name }
    );
    let instantiation = &mut template^.instantiations
        |> OrdMap.get_or_init(args, do_instantiate);
    Log.debug(
        () => (
            let output = @current Output;
            output.write("Instantiation at ");
            Span.print(span);
            output.write(" = ");
            output.write(instantiation^.name);
        )
    );
    instantiation^
);

const parse_instantiate = (root :: Ast.Group) -> Instantiation => (
    let { template, args_ast } = root
        |> AstHelpers.expect_two_children(:Some { "template", "args" });
    let template_name = template
        |> AstHelpers.expect_ident;

    let mut args = ArrayList.new();
    for arg in Ast.iter_list(
        args_ast,
        .binary_rule_name = "comma",
        .trailing_or_leading_rule_name = :Some "trailing comma",
    ) do (
        &mut args |> ArrayList.push_back((@current Compiler).parse_type(arg));
    );
    instantiate(template_name, args, .span = root.span)
);

const parse_instantiate_ty = (root :: Ast.Group) -> Ir.Type => (
    parse_instantiate(root)
        |> instantiation_to_type(.span = root.span)
);
