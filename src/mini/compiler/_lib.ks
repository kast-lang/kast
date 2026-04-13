use (import "./common.ks").*;

use (import "./template.ks").*;
use (import "./type_def.ks").*;
use (import "./type_check.ks").*;
use (import "./type_info.ks").*;

const root_scope = @current_scope;

module:

const Compiler = (
    module:

    const Processable = [T] newtype (
        | :Unprocessed
        | :Processing
        | :Processed T
    );

    const TopLevelItem = newtype {
        .def :: TopLevelItemDef,
        .declaration :: Processable[TopLevelDecl],
        .implementation :: Processable[TopLevelImpl],
    };

    const State = newtype {
        .toplevel_items :: OrdMap.t[String, TopLevelItem],
        .toplevel_items_unprocessed :: Queue.t[String],
        .program :: Ir.Program,
    };

    const Context = @context State;

    const init = () -> State => {
        .toplevel_items = OrdMap.new(),
        .toplevel_items_unprocessed = Queue.new(),
        .program = {
            .types = OrdMap.new(),
            .contexts = OrdMap.new(),
            .consts = OrdMap.new(),
            .consts_order = ArrayList.new(),
            .fns = OrdMap.new(),
        },
    };

    const get_toplevel_decl = (
        name :: String,
    ) -> Option.t[TopLevelDecl] => with_return (
        let ctx = @current Context;
        let item = &ctx.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap_or_else(() => return :None);
        match item^.declaration with (
            | :Processed value => :Some value
            | _ => (
                process_toplevel_item(name);
                get_toplevel_decl(name)
            )
        )
    );

    const get_toplevel_impl = (
        name :: String,
    ) -> Option.t[TopLevelImpl] => with_return (
        let ctx = @current Context;
        let item = &ctx.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap_or_else(() => return :None);
        match item^.implementation with (
            | :Processed value => :Some value
            | _ => (
                process_toplevel_item(name);
                get_toplevel_impl(name)
            )
        )
    );

    const register_type_def = (name :: String, def :: Ir.TypeDef) => (
        &mut (@current Context).program.types
            |> OrdMap.add(name, def);
    );

    const parse_toplevel_item = (ast :: Ast.t) => with_return (
        let mut ctx = @current Context;
        if ast.shape is :Rule { .rule, .root } then (
            if rule.name == "native const" then (
                let { name_ast, ty } = root
                    |> AstHelpers.expect_two_children(:Some { "name", "type" });
                let name = name_ast |> AstHelpers.expect_ident;
                let toplevel_item = {
                    .name,
                    .span = name_ast.span,
                    .ast,
                    .native = true,
                    .setup_contexts = :None,
                };
                add_toplevel_item(toplevel_item);
                return;
            );
            if rule.name == "native const value" then (
                let { name_ast, value } = root
                    |> AstHelpers.expect_two_children(:Some { "name", "value" });
                let name = name_ast |> AstHelpers.expect_ident;
                let toplevel_item = {
                    .name,
                    .span = name_ast.span,
                    .ast = value,
                    .native = true,
                    .setup_contexts = :None,
                };
                add_toplevel_item(toplevel_item);
                return;
            );
        );
        let root = ast |> AstHelpers.expect_rule("const");
        let name_ast = (
            &root.children
                |> Tuple.get_named("name")
        )^
            |> Ast.unwrap_child_value;
        let value = (
            &root.children
                |> Tuple.get_named("value")
        )^
            |> Ast.unwrap_child_value;
        if name_ast.shape is :Rule { .rule, .root } then (
            if rule.name == "type ascribe" then (
                let { name_ast, ty } = root
                    |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                let name = name_ast |> AstHelpers.expect_ident;
                let toplevel_item = {
                    .name,
                    .span = name_ast.span,
                    .ast,
                    .native = false,
                    .setup_contexts = :None,
                };
                add_toplevel_item(toplevel_item);
                return;
            );
        );
        let name = name_ast |> AstHelpers.expect_ident;
        let toplevel_item = {
            .name,
            .span = name_ast.span,
            .ast = value,
            .native = false,
            .setup_contexts = :None,
        };
        add_toplevel_item(toplevel_item);
    );

    const add_toplevel_item = (def :: TopLevelItemDef) => with_return (
        let { .name, .span, ... } = def;
        let mut ctx = @current Context;
        if &ctx.toplevel_items |> OrdMap.get(name) is :Some existing then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("Top level item ");
                    output.write(String.escape(name));
                    output.write(" has already been defined");
                ),
                .span,
                .related = (
                    let mut related = ArrayList.new();
                    let info = {
                        .span = existing^.def.span,
                        .message = () => (
                            let output = @current Output;
                            output.write("Previous definition is here");
                        ),
                    };
                    &mut related |> ArrayList.push_back(info);
                    related
                ),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        let item = {
            .def,
            .declaration = :Unprocessed,
            .implementation = :Unprocessed,
        };
        &mut ctx.toplevel_items |> OrdMap.add(name, item);
        &mut ctx.toplevel_items_unprocessed |> Queue.push(name);
    );

    const process_toplevel_item_impl = (name :: String) => with_return (
        let mut ctx = @current Context;
        let item = () => &mut ctx.toplevel_items
            |> OrdMap.get_mut(name)
            |> Option.unwrap;
        let native = item()^.def.native;
        item()^.declaration = :Processing;
        item()^.implementation = :Processing;
        if item()^.def.ast.shape is :Rule { .rule, .root } then (
            if rule.name == "const" then (
                let decl = process_const_declaration(name, item()^.def.ast);
                item()^.declaration = :Processed :Const decl;
                let value = process_const(name, decl);
                item()^.implementation = :Processed :Const value;
                return;
            );
            if rule.name == "native const" then (
                let decl = process_const_declaration(name, item()^.def.ast);
                item()^.declaration = :Processed :Const decl;
                item()^.implementation = :Processed :NativeConst;
                return;
            );
            if rule.name == "template" then (
                item()^.declaration = :Processed :Template;
                let template = parse_template(name, root);
                item()^.implementation = :Processed :Template template;
                return;
            );
            if rule.name == "type alias" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Alias, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "enum" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Enum, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "struct" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Struct, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "union" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Union, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "opaque_type" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Opaque, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "fn" then (
                let fn_type = process_toplevel_fn_declaration(name, item()^.def.ast);
                item()^.declaration = :Processed :Fn fn_type;
                let fn_def = process_toplevel_fn(name, item()^.def.ast);
                item()^.implementation = :Processed :Fn fn_def;
                return;
            );
            if rule.name == "create_context_type" then (
                item()^.declaration = :Processed :Context;
                let ty = root
                    |> AstHelpers.expect_single_child(:Some "type")
                    |> parse_type;
                item()^.implementation = :Processed :Context ty;
                return;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                (@current Output).write("Unexpected top level item");
            ),
            .span = item()^.def.ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const process_toplevel_item = (name :: String) => with_return (
        let mut ctx = @current Context;
        let item = &ctx.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap;
        let loop_detected = () => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("Detected a loop processing ");
                    output.write(name);
                ),
                .span = item^.def.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        match item^.implementation with (
            | :Processed _ => return
            | :Processing => loop_detected()
            | :Unprocessed => ()
        );
        match item^.declaration with (
            | :Processed _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Internal,
                    .message = () => (
                        let output = @current Output;
                        output.write("Declaration is processed but defintion is not for ");
                        output.write(name);
                    ),
                    .span = item^.def.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
            | :Processing => loop_detected()
            | :Unprocessed => ()
        );
        match item^.def.setup_contexts with (
            | :Some f => (
                f(() => process_toplevel_item_impl(name))
            )
            | :None => (
                process_toplevel_item_impl(name);
            )
        );
        let item = &ctx.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap;
        match item^.implementation with (
            | :Processed item => match item with (
                | :Const expr => (
                    &mut ctx.program.consts |> OrdMap.add(name, expr);
                    &mut ctx.program.consts_order |> ArrayList.push_back(name);
                )
                | :NativeConst => ()
                | :Type def => (
                    &mut ctx.program.types |> OrdMap.add(name, def);
                )
                | :Fn def => (
                    &mut ctx.program.fns |> OrdMap.add(name, def);
                )
                | :Context ty => (
                    &mut ctx.program.contexts |> OrdMap.add(name, ty);
                )
                | :Template _ => ()
            )
            | _ => panic("unreachable")
        )
    );

    const add_source_ast = (state :: &mut State, ast :: Ast.t) => (
        # TODO this will copy state, need context to be ref?
        with Context = state^;
        for item in Ast.iter_list(
            ast,
            .binary_rule_name = "then",
            .trailing_or_leading_rule_name = :Some "stmt",
        ) do (
            parse_toplevel_item(item);
        );
    );

    const ruleset_path = () -> String => (
        "src/mini/syntax.ks"
    );

    const ruleset = () -> SyntaxRuleset.t => (
        let mut lexer = Lexer.new(Source.read(SourcePath.file(ruleset_path())));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        SyntaxParser.parse_syntax_ruleset(&mut token_stream)
    );

    const add_source = (state :: &mut State, source :: Source) => (
        let ruleset = ruleset();
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let parsed = Parser.parse(
            .ruleset,
            .entire_source_span = Source.entire_span(&source),
            .path = source.path,
            .token_stream = &mut token_stream,
        );
        add_source_ast(state, parsed.ast)
    );

    const compile = (mut state :: State) -> Ir.Program => (
        with Context = state;
        with root_scope.Compiler = {
            # TODO we have to copies now, only works because we do js
            .program = state.program,
            .parse_type,
            .register_type_def,
            .find_ident_ty,
            .lookup_type,
            .add_toplevel_item,
            .resolve_type_aliases,
            .get_toplevel_decl,
            .get_toplevel_impl,
        };
        # Empty scope is need for initializing consts (they do name lookups)
        let mut scope = {
            .parent = :None,
            .vars = OrdMap.new(),
        };
        with ScopeContext = scope;
        # Outside of template instantiations
        with TemplateArgsContext = {
            .args = ArrayList.new(),
            .by_name = OrdMap.new(),
        };
        while &mut state.toplevel_items_unprocessed |> Queue.pop is :Some name do (
            process_toplevel_item(name);
        );
        state.program
    );

    const Scope = newtype {
        .parent :: Option.t[Scope],
        .vars :: OrdMap.t[String, Ir.Type],
    };
    const ScopeContext = @context Scope;

    const find_in_scope = (scope :: &Scope, name :: String) -> Option.t[Ir.Type] => (
        match &scope^.vars |> OrdMap.get(name) with (
            | :Some &result => :Some result
            | :None => match scope^.parent with (
                | :Some ref parent => find_in_scope(parent, name)
                | :None => :None
            )
        )
    );

    const print_toplevel_kind = (decl :: TopLevelDecl) => (
        let kind = match decl with (
            | :Fn _ => "function"
            | :Type => "type"
            | :Template => "template"
            | :Const _ => "constant"
            | :Context => "context"
        );
        (@current Output).write(kind);
    );

    const find_ident_ty = (name :: String, .span :: Span) -> Ir.Type => with_return (
        let ctx = @current Context;
        if find_in_scope(&(@current ScopeContext), name) is :Some result then (
            return result;
        );
        if get_toplevel_decl(name) is :Some decl then (
            match decl with (
                | :Fn fn_type => return :Fn fn_type
                | :Const decl => return decl.ty
                | _ => ()
            );
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" is a ");
                    print_toplevel_kind(decl);
                    output.write(", can't be used as expr");
                ),
                .span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic);
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                let output = @current Output;
                output.write(name);
                output.write(" not found in current scope");
            ),
            .span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
    # TODO should take &
    const resolve_type_aliases = (ty :: Ir.Type) -> Ir.Type => with_return (
        match ty with (
            | :Any => :Any
            | :Unit => :Unit
            | :Int32 => :Int32
            | :Int64 => :Int64
            | :Float64 => :Float64
            | :Char => :Char
            | :Bool => :Bool
            | :Native s => :Native s
            | :Named name => (
                let def = &(@current Context).program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                match def^.shape with (
                    | :Alias ty => (
                        Log.trace(
                            () => (
                                let output = @current Output;
                                output.write(name);
                                output.write(" is alias to ");
                                Ir.Print.type_name(&ty);
                            )
                        );
                        resolve_type_aliases(ty)
                    )
                    | _ => :Named name
                )
            )
            | :Ref referenced => :Ref resolve_type_aliases(referenced)
            | :Fn { .args, .result } => (
                let mut resolved_args = ArrayList.new();
                for &arg in &args |> ArrayList.iter do (
                    &mut resolved_args |> ArrayList.push_back(resolve_type_aliases(arg));
                );
                let result = resolve_type_aliases(result);
                :Fn {
                    .args = resolved_args,
                    .result = resolve_type_aliases(result),
                }
            )
        )
    );

    const parse_if = (expected_ty :: Option.t[Ir.Type], root :: Ast.Group) -> ParsedExpr => (
        let cond = (&root.children |> Tuple.get_named("cond"))^
            |> Ast.unwrap_child_value;
        let then_case = (&root.children |> Tuple.get_named("then_case"))^
            |> Ast.unwrap_child_value;
        let else_case = &root.children
            |> Tuple.get_named_opt("else_case")
            |> Option.map(child => child^ |> Ast.unwrap_child_value);
        let cond = parse_expr(:Some :Bool, cond);
        let then_case = parse_expr(expected_ty, then_case);
        let else_case = else_case
            |> Option.map(ast => parse_expr(:Some then_case.ty, ast));
        {
            .shape = :Expr :If { .cond, .then_case, .else_case },
            .ty = then_case.ty,
        }
    );

    const expect_known_type = (
        expected_ty :: Option.t[Ir.Type],
        .span :: Span,
    ) -> Ir.Type => (
        match expected_ty with (
            | :Some ty => ty
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        let output = @current Output;
                        output.write("Unable to infer type");
                    ),
                    .span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const field_ty = (
        obj_ty :: Ir.Type,
        field :: String,
        .field_span :: Span,
    ) -> Ir.Type => with_return (
        let ctx = @current Context;
        let obj_ty = resolve_type_aliases(obj_ty);
        if obj_ty is :Named name then (
            let type_def = &ctx.program.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            let field_ty = match type_def^.shape with (
                | :Union { .variants = ref variants } => (
                    variants |> OrdMap.get(field)
                )
                | :Struct { .fields = ref fields } => (
                    fields |> OrdMap.get(field)
                )
                | _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .message = () => (
                            Ir.Print.type_name(&obj_ty);
                            (@current Output).write(" doesn't have fields");
                        ),
                        .span = field_span,
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            );
            match field_ty with (
                | :Some &ty => return ty
                | :None => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .message = () => (
                            let output = @current Output;
                            Ir.Print.type_name(&obj_ty);
                            output.write(" doesn't have field ");
                            output.write(String.escape(field));
                        ),
                        .span = field_span,
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            )
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                Ir.Print.type_name(&obj_ty);
                (@current Output).write(" doesn't have fields");
            ),
            .span = field_span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const parse_context_type = (ast :: Ast.t) -> { String, Ir.Type } => (
        let ctx = @current Context;
        let context_name = ast |> AstHelpers.expect_ident;
        let context_ty = match &ctx.program.contexts |> OrdMap.get(context_name) with (
            | :Some &ty => ty
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        let output = @current Output;
                        output.write("Unknown context ");
                        output.write(String.escape(context_name));
                    ),
                    .span = ast.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        );
        { context_name, context_ty }
    );

    const parse_expr_impl = (
        expected_ty :: Option.t[Ir.Type],
        ast :: Ast.t,
    ) -> ParsedExpr => (
        let ctx = @current Context;
        let { .shape, .ty } = with_return (
            match ast.shape with (
                | :Empty => return {
                    .shape = :Expr :Unit,
                    .ty = :Unit,
                }
                | :Rule { .rule, .root } => (
                    if rule.name == "instantiate" then (
                        return parse_instantiate_expr(root);
                    );
                    if rule.name == "variant" then (
                        let variant = root
                            |> AstHelpers.expect_single_child(:Some "label")
                            |> AstHelpers.expect_ident;
                        let ty = expected_ty |> expect_known_type(.span = ast.span);
                        check_variant(
                            .variant = { variant, .span = ast.span },
                            .ty = { &ty, .span = ast.span },
                        );
                        return {
                            .shape = :Expr :Variant variant,
                            .ty,
                        };
                    );
                    if rule.name == "==" then (
                        let { lhs, rhs } = root
                            |> AstHelpers.expect_two_children(:None);
                        let enum = parse_expr(:None, lhs);
                        let variant = rhs
                            |> AstHelpers.expect_rule("variant")
                            |> AstHelpers.expect_single_child(:Some "label")
                            |> AstHelpers.expect_ident;
                        check_variant(
                            .variant = { variant, .span = rhs.span },
                            .ty = { &enum.ty, .span = lhs.span },
                        );
                        return {
                            .shape = :Expr :EnumIs { .enum, .variant },
                            .ty = :Bool,
                        };
                    );
                    if rule.name == "type_info" then (
                        let ty = root
                            |> AstHelpers.expect_single_child(:Some "type")
                            |> parse_type;
                        let type_info = type_info(&ty);
                        return {
                            .shape = :Place type_info.shape,
                            .ty = type_info.ty,
                        };
                    );
                    if rule.name == "deref" then (
                        let reference_ast = root |> AstHelpers.expect_single_child(:None);
                        let expected_reference_ty = match expected_ty with (
                            | :None => :None
                            | :Some ty => :Some :Ref ty
                        );
                        let reference = parse_expr(expected_reference_ty, reference_ast);
                        let ty = match resolve_type_aliases(reference.ty) with (
                            | :Ref referenced => referenced
                            | _ => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        (@current Output).write("Expected a reference, got");
                                        Ir.Print.type_name(&reference.ty);
                                    ),
                                    .span = reference_ast.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        return {
                            .shape = :Place :Deref reference,
                            .ty,
                        };
                    );
                    if rule.name == "ref" then (
                        let referenced = root |> AstHelpers.expect_single_child(:None);
                        let expected_referenced_ty = match expected_ty with (
                            | :None => :None
                            | :Some ty => match resolve_type_aliases(ty) with (
                                | :Ref referenced => :Some referenced
                                | _ => (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            (@current Output).write("Expected not a reference but ");
                                            Ir.Print.type_name(&ty);
                                        ),
                                        .span = ast.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                )
                            )
                        );
                        let referenced = parse_place_expr(expected_referenced_ty, referenced);
                        return {
                            .shape = :Expr :Ref referenced,
                            .ty = :Ref referenced.ty
                        };
                    );
                    if rule.name == "record" then (
                        let ty = match expected_ty with (
                            | :Some ty => ty
                            | :None => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        (@current Output).write("Couldn't figure out the type of this record");
                                    ),
                                    .span = ast.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        const Kind = newtype (
                            | :Union
                            | :Struct
                        );
                        let {
                            kind :: Kind,
                            field_types :: OrdMap.t[String, Ir.Type],
                        } = with_return (
                            match resolve_type_aliases(ty) with (
                                | :Named name => (
                                    let def = &ctx.program.types |> OrdMap.get(name) |> Option.unwrap;
                                    match def^.shape with (
                                        | :Struct { .fields } => return { :Struct, fields }
                                        | :Union { .variants } => return { :Union, variants }
                                        | _ => ()
                                    )
                                )
                                | _ => ()
                            );
                            let diagnostic = {
                                .severity = :Error,
                                .source = :Compiler,
                                .message = () => (
                                    let output = @current Output;
                                    output.write("Type ");
                                    Ir.Print.type_name(&ty);
                                    output.write(" can't be used as record");
                                ),
                                .span = ast.span,
                                .related = ArrayList.new(),
                            };
                            Diagnostic.report_and_unwind(diagnostic)
                        );
                        let mut fields_initialized = OrdMap.new();
                        let mut fields = ArrayList.new();
                        for field in Ast.iter_list(
                            root |> AstHelpers.expect_single_child(:None),
                            .binary_rule_name = "comma",
                            .trailing_or_leading_rule_name = :Some "trailing comma",
                        ) do (
                            let { name_ast, value } = field
                                |> AstHelpers.expect_rule("field init")
                                |> AstHelpers.expect_two_children(:Some { "label", "value" });
                            let name = name_ast |> AstHelpers.expect_ident;
                            let field_ty = match &field_types |> OrdMap.get(name) with (
                                | :Some &ty => ty
                                | :None => (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            let output = @current Output;
                                            output.write("Type ");
                                            Ir.Print.type_name(&ty);
                                            output.write("doesn't have field ");
                                            output.write(String.escape(name));
                                        ),
                                        .span = name_ast.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                )
                            );
                            if &fields_initialized |> OrdMap.get(name) is :Some &other_span then (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        let output = @current Output;
                                        output.write("Field ");
                                        output.write(String.escape(name));
                                        output.write(" has already been initialized");
                                    ),
                                    .span = name_ast.span,
                                    .related = (
                                        let mut related = ArrayList.new();
                                        let info = {
                                            .span = other_span,
                                            .message = () => (
                                                let output = @current Output;
                                                output.write("Previously field was initialized here");
                                            ),
                                        };
                                        &mut related |> ArrayList.push_back(info);
                                        related
                                    ),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            );
                            &mut fields_initialized |> OrdMap.add(name, name_ast.span);
                            let value = parse_expr(:Some field_ty, value);
                            &mut fields |> ArrayList.push_back({ .name, .value });
                        );
                        return {
                            .shape = :Expr :Record fields,
                            .ty,
                        };
                    );
                    if rule.name == "fn" then (
                        let fn = parse_fn_def(
                            ast,
                            .parent_scope = :Some (@current ScopeContext),
                        );
                        let fn_ty = {
                            .args = (
                                let mut args = ArrayList.new();
                                for arg in &fn.args |> ArrayList.iter do (
                                    &mut args |> ArrayList.push_back(arg^.ty);
                                );
                                args
                            ),
                            .result = fn.result_ty,
                        };
                        return {
                            .shape = :Expr :Fn fn,
                            .ty = :Fn fn_ty,
                        };
                    );
                    if rule.name == "field" then (
                        let { obj, field } = root
                            |> AstHelpers.expect_two_children(:Some { "obj", "field" });
                        let field_span = field.span;
                        let field = field |> AstHelpers.expect_ident;
                        let obj = parse_place_expr(:None, obj);
                        let field_ty = field_ty(obj.ty, field, .field_span);
                        return {
                            .shape = :Place :Field {
                                .obj,
                                .field,
                            },
                            .ty = field_ty,
                        };
                    );
                    if rule.name == "assign" then (
                        let { assignee, value } = root
                            |> AstHelpers.expect_two_children(:Some { "assignee", "value" });
                        let assignee = parse_place_expr(:None, assignee);
                        let value = parse_expr(:Some assignee.ty, value);
                        return {
                            .shape = :Expr :Assign {
                                .assignee,
                                .value,
                            },
                            .ty = :Unit,
                        };
                    );
                    if rule.name == "uninitialized" then (
                        let expected_ty = expect_known_type(expected_ty, .span = ast.span);
                        return { .shape = :Expr :Uninitialized, .ty = expected_ty };
                    );
                    if rule.name == "type ascribe" then (
                        let { expr, ty } = root
                            |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                        let ty = parse_type(ty);
                        let expr = parse_expr_impl(:Some ty, expr);
                        return { .shape = expr.shape, .ty = expr.ty };
                    );
                    if rule.name == "loop" then (
                        let body = root |> AstHelpers.expect_single_child(:None);
                        return {
                            .shape = :Expr :Loop parse_expr(:Some :Unit, body),
                            .ty = :Unit,
                        };
                    );
                    if rule.name == "if" then (
                        return parse_if(expected_ty, root);
                    );
                    if rule.name == "if_without_else" then (
                        return parse_if(expected_ty, root);
                    );
                    if rule.name == "let" then (
                        let { pattern, value } = root
                            |> AstHelpers.expect_two_children(:Some { "pattern", "value" });
                        let { name, expected_ty } = with_return (
                            if pattern.shape is :Rule { .rule, .root } then (
                                if rule.name == "type ascribe" then (
                                    let { name, ty } = root
                                        |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                                    let name = name |> AstHelpers.expect_ident;
                                    return { name, :Some parse_type(ty) };
                                );
                            );
                            let name = pattern |> AstHelpers.expect_ident;
                            { name, :None }
                        );
                        let value = parse_expr(expected_ty, value);
                        &mut (@current ScopeContext).vars |> OrdMap.add(name, value.ty);
                        return {
                            .shape = :Expr :Let { .name, .value },
                            .ty = :Unit,
                        };
                    );
                    if rule.name == "native" then (
                        let inner = root |> AstHelpers.expect_single_child(:None);
                        let mut native_parts = ArrayList.new();
                        match inner.shape with (
                            | :InterpolatedString { .parts, ... } => (
                                for part in parts |> ArrayList.into_iter do (
                                    match part with (
                                        | :Content s => (
                                            &mut native_parts |> ArrayList.push_back(:Raw s.contents);
                                        )
                                        | :Interpolated { .ast, ... } => (
                                            let part = :Interpolated parse_expr(:None, ast);
                                            &mut native_parts |> ArrayList.push_back(part);
                                        )
                                    )
                                )
                            )
                            | :Token { .shape = :String s, ... } => (
                                &mut native_parts |> ArrayList.push_back(:Raw s.contents);
                            )
                            | _ => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        (@current Output).write("Expected a native string");
                                    ),
                                    .span = inner.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        return {
                            .shape = :Expr :Native { .parts = native_parts },
                            .ty = expected_ty |> Option.unwrap_or(:Unit),
                        };
                    );
                    if rule.name == "stmt" then (
                        let inner = root |> AstHelpers.expect_single_child(:None);
                        return {
                            .shape = :Expr :Stmt parse_expr(:None, inner),
                            .ty = :Unit,
                        };
                    );
                    if rule.name == "scope" then (
                        with ScopeContext = {
                            .parent = :Some (@current ScopeContext),
                            .vars = OrdMap.new(),
                        };
                        let inner = root |> AstHelpers.expect_single_child(:None);
                        let inner = parse_expr(expected_ty, inner);
                        return {
                            .shape = :Expr :Scope inner,
                            .ty = inner.ty,
                        };
                    );
                    if rule.name == "inject_context" then (
                        let { context_type, value } = root
                            |> AstHelpers.expect_two_children(:Some { "context_type", "value" });
                        let { context_name, context_ty } = parse_context_type(context_type);
                        return {
                            .shape = :Expr :InjectContext {
                                .name = context_name,
                                .value = parse_expr(:Some context_ty, value),
                            },
                            .ty = :Unit,
                        };

                    );
                    if rule.name == "current_context" then (
                        let context_type = root
                            |> AstHelpers.expect_single_child(:Some "context_type");
                        let { context_name, context_ty } = parse_context_type(context_type);
                        return {
                            .shape = :Place :CurrentContext context_name,
                            .ty = context_ty,
                        };
                    );
                    if rule.name == "then" then (
                        let mut last_expr_ty = :Unit;
                        let mut expr_asts = ArrayList.new();
                        for expr in Ast.iter_list(
                            ast,
                            .binary_rule_name = "then",
                            .trailing_or_leading_rule_name = :None,
                        ) do (
                            &mut expr_asts |> ArrayList.push_back(expr);
                        );
                        let mut exprs = ArrayList.new();
                        for { i, &expr } in &expr_asts |> ArrayList.iter |> std.iter.enumerate do (
                            let expected_ty = if i + 1 < ArrayList.length(&expr_asts) then (
                                :None
                            ) else (
                                expected_ty
                            );
                            let expr = parse_expr(expected_ty, expr);
                            last_expr_ty = expr.ty;
                            &mut exprs |> ArrayList.push_back(expr);
                        );
                        return {
                            .shape = :Expr :Then exprs,
                            .ty = last_expr_ty,
                        };
                    );
                    if rule.name == "apply" then (
                        let f_ast = (&root.children |> Tuple.get_named("f"))^
                            |> Ast.unwrap_child_value;
                        let args_ast = (&root.children |> Tuple.get_unnamed(0))^
                            |> Ast.unwrap_child_group;
                        let args_ast = args_ast |> AstHelpers.expect_single_child(:Some "args");
                        let f = parse_expr(:None, f_ast);
                        let f_type = match resolve_type_aliases(f.ty) with (
                            | :Fn ty => ty
                            | _ => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        (@current Output).write("Expected a function, got ");
                                        Ir.Print.type_name(&f.ty);
                                    ),
                                    .span = f_ast.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        let mut arg_asts = ArrayList.new();
                        for arg in Ast.iter_list(
                            args_ast,
                            .binary_rule_name = "comma",
                            .trailing_or_leading_rule_name = :Some "trailing comma",
                        ) do (
                            &mut arg_asts |> ArrayList.push_back(arg);
                        );

                        let actual_arg_count = &arg_asts |> ArrayList.length;
                        let expected_arg_count = &f_type.args |> ArrayList.length;
                        if actual_arg_count != expected_arg_count then (
                            let diagnostic = {
                                .severity = :Error,
                                .source = :Compiler,
                                .message = () => (
                                    let output = @current Output;
                                    output.write("Expected ");
                                    output.write(to_string(expected_arg_count));
                                    output.write(" args, got ");
                                    output.write(to_string(actual_arg_count));
                                ),
                                .span = args_ast.span,
                                .related = ArrayList.new(),
                            };
                            Diagnostic.report_and_unwind(diagnostic)
                        );
                        let mut args = ArrayList.new();
                        for { i, arg_ast } in arg_asts |> ArrayList.into_iter |> std.iter.enumerate do (
                            let expected_arg_ty = :Some (&f_type.args |> ArrayList.at(i))^;
                            let arg = parse_expr(expected_arg_ty, arg_ast);
                            &mut args |> ArrayList.push_back(arg);
                        );
                        return {
                            .shape = :Expr :Apply { .f, .args },
                            .ty = f_type.result,
                        };
                    );
                )
                | :Token token => (
                    match token.shape with (
                        | :Ident ident => (
                            return {
                                .shape = :Place :Ident ident.name,
                                .ty = find_ident_ty(ident.name, .span = token.span),
                            }
                        )
                        | :Number { .raw, ... } => (
                            let ty = expect_known_type(expected_ty, .span = token.span);
                            # TODO catch parse errors
                            let literal = match resolve_type_aliases(ty) with (
                                | :Int32 => :Int32 parse(raw)
                                | :Int64 => :Int64 parse(raw)
                                | :Float64 => :Float64 parse(raw)
                                | _ => (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            (@current Output).write("Number literal can't be ");
                                            Ir.Print.type_name(&ty);
                                        ),
                                        .span = token.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                )
                            );
                            return {
                                .shape = :Expr :Literal literal,
                                .ty,
                            };
                        )
                        | :String str => (
                            let open_raw = Token.raw(str.open);
                            let { ty, literal } = if open_raw == "\"" then (
                                { :Named "StringView", :String str.contents }
                            ) else if open_raw == "'" then (
                                if String.length(str.contents) == 0 then (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            (@current Output).write("Char literal can't be empty");
                                        ),
                                        .span = token.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                );
                                let c = String.at(str.contents, 0);
                                if Char.string_encoding_len(c) != String.length(str.contents) then (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            (@current Output).write("Char literal must have exactly 1 char");
                                        ),
                                        .span = token.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                );
                                { :Char, :Char c }
                            ) else (
                                panic("Unkown string delimeter")
                            );
                            return {
                                .shape = :Expr :Literal literal,
                                .ty,
                            };
                        )
                        | _ => ()
                    )
                )
                | _ => ()
            );
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    (@current Output).write("Expected an expr, got ");
                    Highlight.print_single_line(&ast);
                ),
                .span = ast.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        let ty = if expected_ty is :Some expected_ty then (
            type_check(
                .expected = expected_ty,
                .actual = ty,
                .span = ast.span
            );
            expected_ty
        ) else (
            ty
        );
        { .shape, .ty }
    );

    const parse_expr = (
        expected_ty :: Option.t[Ir.Type],
        ast :: Ast.t,
    ) -> Ir.Expr => (
        let { .shape, .ty } = parse_expr_impl(expected_ty, ast);
        let shape = match shape with (
            | :Expr shape => shape
            | :Place shape => :Claim { .shape, .ty, .span = ast.span }
        );
        { .shape, .ty, .span = ast.span }
    );

    const parse_place_expr = (
        expected_ty :: Option.t[Ir.Type],
        ast :: Ast.t,
    ) -> Ir.PlaceExpr => (
        let { .shape, .ty } = parse_expr_impl(expected_ty, ast);
        let shape = match shape with (
            | :Place shape => shape
            | :Expr shape => :Temp { .shape, .ty, .span = ast.span }
        );
        { .shape, .ty, .span = ast.span }
    );

    const lookup_type = (name :: String, .span :: Span) -> Ir.Type => with_return (
        if &(@current TemplateArgsContext).by_name |> OrdMap.get(name) is :Some ty then (
            return ty^;
        );
        if get_toplevel_decl(name) is :Some decl then (
            if decl is :Type then (
                return :Named name;
            );
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write(String.escape(name));
                    output.write("is ");
                    print_toplevel_kind(decl);
                    output.write(", not a type");
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
                output.write("Type ");
                output.write(String.escape(name));
                output.write(" not found");
            ),
            .span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const parse_type = (ast :: Ast.t) -> Ir.Type => with_return (
        match ast.shape with (
            | :Empty => return :Unit
            | :Rule { .rule, .root } => (
                if rule.name == "instantiate" then (
                    return parse_instantiate_ty(root);
                );
                if rule.name == "native" then (
                    let raw = root
                        |> AstHelpers.expect_single_child(:None)
                        |> AstHelpers.expect_string;
                    return :Native raw;
                );
                if rule.name == "ref" then (
                    let referenced = root |> AstHelpers.expect_single_child(:None);
                    return :Ref parse_type(referenced);
                );
                if rule.name == "fn_type" then (
                    let { arg_asts, result } = root
                        |> AstHelpers.expect_two_children(:Some { "args", "result" });
                    let arg_asts = arg_asts
                        |> AstHelpers.expect_rule("scope")
                        |> AstHelpers.expect_single_child(:None);
                    let mut args = ArrayList.new();
                    for arg_ast in Ast.iter_list(
                        arg_asts,
                        .binary_rule_name = "comma",
                        .trailing_or_leading_rule_name = :Some "trailing comma",
                    ) do (
                        if arg_ast.shape is :Rule { .rule, .root } then (
                            if rule.name == "type ascribe" then (
                                let { name, ty } = root
                                    |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                                let name = name |> AstHelpers.expect_ident;
                                &mut args |> ArrayList.push_back(parse_type(ty));
                                continue;
                            );
                        );
                        &mut args |> ArrayList.push_back(parse_type(arg_ast));
                    );
                    let result = parse_type(result);
                    return :Fn { .args, .result };
                );
                if rule.name == "scope" then (
                    let inner = root
                        |> AstHelpers.expect_single_child(:None);
                    return parse_type(inner);
                );
            )
            | :Token token => match token.shape with (
                | :Ident ident => (
                    let name = ident.name;
                    if name == "Any" then return :Any;
                    if name == "Unit" then return :Unit;
                    if name == "Bool" then return :Bool;
                    if name == "Int32" then return :Int32;
                    if name == "Int64" then return :Int64;
                    if name == "Float64" then return :Float64;
                    if name == "Char" then return :Char;
                    return lookup_type(name, .span = token.span);
                )
                | _ => ()
            )
            | _ => ()
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                (@current Output).write("Expected a type, got ");
                Highlight.print_single_line(&ast);
            ),
            .span = ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const process_toplevel_fn_declaration = (
        name :: String,
        def :: Ast.t,
    ) -> Ir.FnType => (
        let { args, result_ty, body } = def
            |> AstHelpers.expect_rule("fn")
            |> AstHelpers.expect_three_children("args", "result_ty", "body");
        let args = args
            |> AstHelpers.expect_rule("scope")
            |> AstHelpers.expect_single_child(:None);
        let mut arg_types = ArrayList.new();
        for arg in Ast.iter_list(
            args,
            .binary_rule_name = "comma",
            .trailing_or_leading_rule_name = :Some "trailing comma",
        ) do (
            let { name, ty } = arg
                |> AstHelpers.expect_rule("type ascribe")
                |> AstHelpers.expect_two_children(:Some { "expr", "type" });
            let ty = parse_type(ty);
            &mut arg_types |> ArrayList.push_back(ty);
        );
        let result_ty = parse_type(result_ty);
        let mut ctx = @current Context;
        let fn_type = {
            .args = arg_types,
            .result = result_ty,
        };
        Log.debug(
            () => (
                let output = @current Output;
                output.write("fn ");
                output.write(name);
                output.write(" :: ");
                Ir.Print.fn_type(&fn_type);
            )
        );
        fn_type
    );

    const process_const_declaration = (
        name :: String,
        ast :: Ast.t,
    ) -> ConstDeclaration => with_return (
        if ast.shape is :Rule { .rule, .root } then (
            if rule.name == "native const" then (
                let { name, ty } = root
                    |> AstHelpers.expect_two_children(:Some { "name", "type" });
                return {
                    .ty = parse_type(ty),
                    .value = :None,
                };
            );
        );
        let { name_and_type, value } = ast
            |> AstHelpers.expect_rule("const")
            |> AstHelpers.expect_two_children(:Some { "name", "value" });
        let { name, ty } = name_and_type
            |> AstHelpers.expect_rule("type ascribe")
            |> AstHelpers.expect_two_children(:Some { "expr", "type" });
        {
            .ty = parse_type(ty),
            .value = :Some value,
        }
    );

    const process_const = (
        name :: String,
        decl :: ConstDeclaration,
    ) -> Ir.Expr => (
        if decl.value is :Some value then (
            parse_expr(:Some decl.ty, value)
        ) else (
            panic("Const decl has no value (native const), we aren't supposed to process it")
        )
    );

    const parse_fn_def = (
        def :: Ast.t,
        .parent_scope :: Option.t[Scope],
    ) -> Ir.FnDef => (
        let { args_ast, result_ty, body } = def
            |> AstHelpers.expect_rule("fn")
            |> AstHelpers.expect_three_children("args", "result_ty", "body");
        let args_ast = args_ast
            |> AstHelpers.expect_rule("scope")
            |> AstHelpers.expect_single_child(:None);
        let mut args :: ArrayList.t[Ir.FnArg] = ArrayList.new();
        for arg in Ast.iter_list(
            args_ast,
            .binary_rule_name = "comma",
            .trailing_or_leading_rule_name = :Some "trailing comma",
        ) do (
            let { name, ty } = arg
                |> AstHelpers.expect_rule("type ascribe")
                |> AstHelpers.expect_two_children(:Some { "expr", "type" });
            let name = name |> AstHelpers.expect_ident;
            let ty = parse_type(ty);
            &mut args |> ArrayList.push_back({ .name, .ty });
        );
        let result_ty = parse_type(result_ty);
        let mut scope = {
            .parent = parent_scope,
            .vars = OrdMap.new(),
        };
        for arg in &args |> ArrayList.iter do (
            &mut scope.vars |> OrdMap.add(arg^.name, arg^.ty);
        );
        with ScopeContext = scope;
        let body = parse_expr(:Some result_ty, body);
        {
            .args,
            .result_ty = result_ty,
            .body,
            .span = def.span,
        }
    );

    const process_toplevel_fn = (
        name :: String,
        def :: Ast.t,
    ) -> Ir.FnDef => (
        parse_fn_def(def, .parent_scope = :None)
    );
);
