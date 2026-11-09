use (import "./common.ks").*;

module:

const TemplateArgsContext = @context type (&TemplateArgs);

impl TemplateArgs as std.cmp.Ord = {
    .compare = (a, b) => with_return (
        let len_a = &a^.args^ |> ArrayList.length;
        let len_b = &b^.args^ |> ArrayList.length;
        let len_ord = std.cmp.default_compare(&len_a, &len_b);
        if len_ord is :Equal then () else (
            return len_ord;
        );
        for i in 0..len_a do (
            let ty_a = &a^.args^.[i];
            let ty_b = &b^.args^.[i];
            let ty_ord = std.cmp.default_compare(ty_a, ty_b);
            if ty_ord is :Equal then () else (
                return ty_ord;
            );
        );
        :Equal
    ),
};

const parse_template = (name :: &RcStr, root :: &Ast.Group) -> Template => (
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
        .def = def^,
        .instantiations = OrdMap.new(),
    }
);

const instantiation_to_type = (
    instantiation :: Instantiation,
    .span :: Span,
) -> Ty => (
    let ty = (@current Compiler).lookup_type(&instantiation.name, .span);
    let template_name = &instantiation.template_name |> RcStr.as_str;
    if template_name == "UnwindToken" then (
        {
            .shape = Rc.new(:UnwindToken {
                .repr = ty,
                .result_ty = instantiation.template_args^.[0],
            }),
            .alias_name = :None,
        }
    ) else if template_name == "Array" then (
        {
            .shape = Rc.new(:Array {
                .repr = ty,
                .element_ty = instantiation.template_args^.[0],
            }),
            .alias_name = :None,
        }
    ) else if template_name == "DelimitedContinuationToken" then (
        {
            .shape = Rc.new(:DelimitedContinuationToken {
                .repr = ty,
                .result_ty = instantiation.template_args^.[0],
            }),
            .alias_name = :None,
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
        .ty = (@current Compiler).find_ident_ty(&instantiation.name, .span),
    }
);

const instantiate_ty = (
    template_name :: &RcStr,
    args :: Rc.t[ArrayList.t[Ir.Type]],
    .span :: Span,
) -> Ir.Type => (
    instantiate(template_name, args, .span)
        |> instantiation_to_type(.span)
);

const find_template = (
    name :: &RcStr,
    .span :: Span,
) -> &mut Template => with_return (
    if (@current Compiler).get_toplevel_impl(name) is :Some item then (
        if item^ is :Template ref mut template then (
            return template;
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write_escaped(name);
                locked_output.write_str(" is not a template");
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
            locked_output.write_str("Could not find template named ");
            locked_output.write_escaped(name);
        ),
        .span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);

const instantiate = (
    template_name :: &RcStr,
    args :: Rc.t[ArrayList.t[Ir.Type]],
    .span :: Span,
) -> Instantiation => (
    let template = find_template(template_name, .span);
    let expected_args_len = &template^.arg_names |> ArrayList.length;
    Log.debug!("calculating template args");
    let args :: TemplateArgs = (
        let mut by_name = OrdMap.new();
        let args_len = &args^ |> ArrayList.length;
        let expected_args_len = &template^.arg_names |> ArrayList.length;
        if args_len != expected_args_len then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    locked_output.write_fmt!("Template expected \(expected_args_len) args, got \(args_len)");
                ),
                .span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic);
        );
        for { i, &arg } in &args^ |> ArrayList.iter |> std.iter.enumerate do (
            let arg_name = template^.arg_names.[i];
            &mut by_name |> OrdMap.add(arg_name, arg);
        );
        { .args, .by_name = Rc.new(by_name) }
    );
    Log.debug!("calculated template args");
    let do_instantiate = () -> Instantiation => with_return (
        let name = RcStr.from_String(output_to_string(() => (
            locked_output.write(template_name);
            for arg in &args.args^ |> ArrayList.iter do (
                locked_output.write_str("_");
                Ir.Print.type_name_as_ident(arg);
            );
        )));
        Log.debug!("instantiating \(name)");
        let toplevel_item = {
            .name,
            .span,
            .ast = template^.def,
            .native = false,
            .setup_contexts = :Some (
                @move f => (
                    with TemplateArgsContext = &args;
                    f()
                )
            ),
        };
        (@current Compiler).add_toplevel_item(toplevel_item);
        { .template_name = template_name^, .template_args = args.args, .name }
    );
    let instantiation = &mut template^.instantiations
        |> OrdMap.get_or_init(args, do_instantiate);
    Log.debug_fn(() => (
        locked_output.write_str("Instantiation at ");
        Span.print(&span);
        locked_output.write_str(" = ");
        locked_output.write(&instantiation^.name);
    ));
    instantiation^
);

const parse_instantiate = (root :: &Ast.Group) -> Instantiation => (
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
    instantiate(&template_name, Rc.new(args), .span = root^.span)
);

const parse_instantiate_ty = (root :: &Ast.Group) -> Ir.Type => (
    parse_instantiate(root)
        |> instantiation_to_type(.span = root^.span)
);
