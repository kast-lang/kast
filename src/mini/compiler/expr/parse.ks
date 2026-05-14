use (import "../common.ks").*;
use (import "../template.ks").*;
use (import "../fn.ks").*;
use (import "../context.ks").*;
use (import "../scope.ks").*;
use (import "../type/inference.ks").*;
use (import "../type/check.ks").*;
use (import "../type/parse.ks").*;
use (import "../type/info.ks").*;

module:

const parse_if = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
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

const parse_variant = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let variant = root
        |> AstHelpers.expect_single_child(:Some "label")
        |> AstHelpers.expect_ident;
    let ty = expected_ty |> expect_known_type(.span = ast.span);
    check_variant(
        .variant = { variant, .span = ast.span },
        .ty = { &ty, .span = ast.span },
    );
    {
        .shape = :Expr :Variant variant,
        .ty,
    }
);

const parse_instantiate_expr = (
    expected_ty :: Option.t[Ty],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    parse_instantiate(root)
        |> instantiation_to_expr(.span = root.span)
);

const parse_equals = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
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
    {
        .shape = :Expr :EnumIs { .enum, .variant },
        .ty = :Bool,
    }
);

const parse_type_info = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let ty = root
        |> AstHelpers.expect_single_child(:Some "type")
        |> parse_type;
    let type_info_const_name = type_info_const_name(&ty);
    {
        .shape = :Place :Ident type_info_const_name,
        .ty = :Named "TypeInfo",
    }
);

const parse_deref = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let reference_ast = root |> AstHelpers.expect_single_child(:None);
    let expected_reference_ty = match expected_ty with (
        | :None => :None
        | :Some ty => :Some :Ref ty
    );
    let reference = parse_expr(expected_reference_ty, reference_ast);
    let ty = match (@current Compiler).resolve_type_aliases(reference.ty) with (
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
    {
        .shape = :Place :Deref reference,
        .ty,
    }
);

const parse_ref = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let referenced = root |> AstHelpers.expect_single_child(:None);
    let expected_referenced_ty = match expected_ty with (
        | :None => :None
        | :Some ty => match (@current Compiler).resolve_type_aliases(ty) with (
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
    {
        .shape = :Expr :Ref referenced,
        .ty = :Ref referenced.ty
    }
);

const parse_list = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let element_ty = match expected_ty with (
        | :Some :List { .element_ty, ... } => element_ty
        | :Some other => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    Ir.Print.type_name(&other);
                    (@current Output).write(" is not a list");
                ),
                .span = ast.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
        | :None => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    (@current Output).write("Couldn't figure out the type of this list");
                ),
                .span = ast.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    );
    let mut elements = ArrayList.new();
    for element in Ast.iter_list(
        root |> AstHelpers.expect_single_child(:None),
        .binary_rule_name = "comma",
        .trailing_or_leading_rule_name = :Some "trailing comma",
    ) do (
        let element = parse_expr(:Some element_ty, element);
        &mut elements |> ArrayList.push_back(element);
    );
    {
        .shape = :Expr :List elements,
        .ty = instantiate_ty(
            "List",
            single_element_list(element_ty),
            .span = ast.span,
        ),
    }
);

const parse_record = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => with_return (
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
        match ty_repr(ty) with (
            | :Named name => (
                let def = &(@current Compiler).program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
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
    {
        .shape = :Expr :Record fields,
        .ty,
    }
);

const parse_fn = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let fn = parse_fn_def(
        ast,
        .parent_scope = :Some (@current ScopeContext),
    );
    let fn_ty = {
        .is_closure = true, # TODO
        .call_convention = :None,
        .args = (
            let mut args = ArrayList.new();
            for arg in &fn.args |> ArrayList.iter do (
                &mut args |> ArrayList.push_back(arg^.ty);
            );
            args
        ),
        .result = fn.result_ty,
    };
    {
        .shape = :Expr :Fn fn,
        .ty = :Fn fn_ty,
    }
);

const parse_field = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let { obj, field } = root
        |> AstHelpers.expect_two_children(:Some { "obj", "field" });
    let field_span = field.span;
    let field = field |> AstHelpers.expect_ident;
    let obj = parse_place_expr(:None, obj);
    let field_ty = field_ty(obj.ty, field, .field_span);
    {
        .shape = :Place :Field {
            .obj,
            .field,
        },
        .ty = field_ty,
    }
);

const parse_assign = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let { assignee, value } = root
        |> AstHelpers.expect_two_children(:Some { "assignee", "value" });
    let assignee = parse_place_expr(:None, assignee);
    let value = parse_expr(:Some assignee.ty, value);
    {
        .shape = :Expr :Assign {
            .assignee,
            .value,
        },
        .ty = :Unit,
    }
);

const parse_uninitialized = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    {
        .shape = :Expr :Uninitialized,
        .ty = expect_known_type(expected_ty, .span = ast.span),
    }
);

const parse_type_ascribe = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let { expr, ty } = root
        |> AstHelpers.expect_two_children(:Some { "expr", "type" });
    let ty = parse_type(ty);
    let expr = parse_expr_impl(:Some ty, expr);
    { .shape = expr.shape, .ty = expr.ty }
);

const parse_defer = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let expr = root
        |> AstHelpers.expect_single_child(:None);
    {
        .shape = :Expr :Defer parse_expr(:Some :Unit, expr),
        .ty = :Unit,
    }
);

const parse_unwind = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let { token_ast, value } = root
        |> AstHelpers.expect_two_children(:Some { "token", "value" });
    let token = parse_expr(:None, token_ast);
    if token.ty is :Ref :UnwindToken { .result_ty, ... } then (
        let value = parse_expr(:Some result_ty, value);
        {
            .shape = :Expr :Unwind {
                .token,
                .value,
            },
            .ty = expected_ty |> Option.unwrap_or(:Unit),
        }
    ) else (
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                (@current Output).write("Expected an reference to unwind token, got ");
                Ir.Print.type_name(&token.ty);
            ),
            .span = token_ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    )
);

const parse_unwindable = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let { token_ast, body } = root
        |> AstHelpers.expect_two_children(:Some { "token", "body" });
    let token = token_ast
        |> AstHelpers.expect_ident;
    with ScopeContext = {
        .parent = :Some (@current ScopeContext),
        .vars = OrdMap.new(),
        .found_in_parent = (...) => (),
    };
    let result_ty = expected_ty |> Option.unwrap_or(:Unit);
    let token_ty_repr = instantiate_ty(
        "UnwindToken",
        single_element_list(result_ty),
        .span = token_ast.span,
    );
    let token_ty = :Ref :UnwindToken {
        .repr = token_ty_repr,
        .result_ty,
    };
    &mut (@current ScopeContext).vars
        |> OrdMap.add(token, token_ty);
    let body = parse_expr(:Some result_ty, body);
    {
        .shape = :Expr :Unwindable {
            .token_ty_repr,
            .token,
            .body,
        },
        .ty = result_ty,
    }
);

const parse_capture_continuation = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let { token_ast, continuation, body } = root
        |> AstHelpers.expect_three_children("token", "continuation", "body");
    let token = parse_expr(:None, token_ast);
    if token.ty is :Ref :DelimitedContinuationToken { .result_ty, ... } then (
        let continuation_ty :: Ir.Type = instantiate_ty(
            "Continuation",
            single_element_list(result_ty),
            .span = ast.span,
        );
        let continuation = continuation |> AstHelpers.expect_ident;
        with ScopeContext = {
            .parent = :Some (@current ScopeContext),
            .vars = OrdMap.new(),
            .found_in_parent = (...) => (),
        };
        &mut (@current ScopeContext).vars
            |> OrdMap.add(continuation, continuation_ty);
        let body = parse_expr(:Some result_ty, body);
        {
            .shape = :Expr :CaptureContinuation {
                .token,
                .continuation_ty_repr = continuation_ty,
                .continuation,
                .resume_fn = instantiate(
                    "resume_delimited_continuation_token", 
                    single_element_list(result_ty),
                    .span = ast.span,
                ).name,
                .body,
            },
            .ty = :Unit,
        }
    ) else (
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                (@current Output).write("Expected an reference to delimited continuation token, got ");
                Ir.Print.type_name(&token.ty);
            ),
            .span = token_ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    )
);

const parse_delimited_continuation = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let capture_mode = :ByRef; # TODO
    let { token_ast, body } = root
        |> AstHelpers.expect_two_children(:Some { "token", "body" });
    let token = token_ast
        |> AstHelpers.expect_ident;
    let mut captures = OrdMap.new();
    with ScopeContext = {
        .parent = :Some (@current ScopeContext),
        .vars = OrdMap.new(),
        .found_in_parent = (name, ty) => (
            &mut captures |> OrdMap.add(name, ty);
        ),
    };
    let result_ty = expected_ty |> Option.unwrap_or(:Unit);
    let token_ty_repr = instantiate_ty(
        "DelimitedContinuationToken",
        single_element_list(result_ty),
        .span = token_ast.span,
    );
    let token_ty = :Ref :DelimitedContinuationToken {
        .repr = token_ty_repr,
        .result_ty,
    };
    &mut (@current ScopeContext).vars
        |> OrdMap.add(token, token_ty);
    let body = parse_expr(:Some result_ty, body);
    {
        .shape = :Expr :DelimitedContinuation {
            .capture_mode,
            .captures,
            .resume_fn = instantiate(
                "resume_delimited_continuation_token", 
                single_element_list(result_ty),
                .span = ast.span,
            ).name,
            .token_ty_repr,
            .token,
            .body,
        },
        .ty = result_ty,
    }
);

const parse_loop = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let body = root |> AstHelpers.expect_single_child(:None);
    {
        .shape = :Expr :Loop parse_expr(:Some :Unit, body),
        .ty = :Unit,
    }
);

const parse_let = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
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
    {
        .shape = :Expr :Let { .name, .value },
        .ty = :Unit,
    }
);

const parse_native = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
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
    {
        .shape = :Expr :Native { .parts = native_parts },
        .ty = expected_ty |> Option.unwrap_or(:Unit),
    }
);

const parse_stmt = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let inner = root |> AstHelpers.expect_single_child(:None);
    {
        .shape = :Expr :Stmt parse_expr(:None, inner),
        .ty = :Unit,
    }
);

const parse_scope = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    with ScopeContext = {
        .parent = :Some (@current ScopeContext),
        .vars = OrdMap.new(),
        .found_in_parent = (...) => (),
    };
    let inner = root |> AstHelpers.expect_single_child(:None);
    let inner = parse_expr(expected_ty, inner);
    {
        .shape = :Expr :Scope inner,
        .ty = inner.ty,
    }
);

const parse_inject_context = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let { context_type, value } = root
        |> AstHelpers.expect_two_children(:Some { "context_type", "value" });
    let { context_name, context_ty } = parse_context_type(context_type);
    {
        .shape = :Expr :InjectContext {
            .name = context_name,
            .value = parse_expr(:Some context_ty, value),
        },
        .ty = :Unit,
    }
);

const parse_current_context = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let context_type = root
        |> AstHelpers.expect_single_child(:Some "context_type");
    let { context_name, context_ty } = parse_context_type(context_type);
    {
        .shape = :Place :CurrentContext context_name,
        .ty = context_ty,
    }
);

const parse_then = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
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
    {
        .shape = :Expr :Then exprs,
        .ty = last_expr_ty,
    }
);

const parse_apply = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let f_ast = (&root.children |> Tuple.get_named("f"))^
        |> Ast.unwrap_child_value;
    let args_ast = (&root.children |> Tuple.get_unnamed(0))^
        |> Ast.unwrap_child_group;
    let args_ast = args_ast |> AstHelpers.expect_single_child(:Some "args");
    let f = parse_expr(:None, f_ast);
    let f_type = match (@current Compiler).resolve_type_aliases(f.ty) with (
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
    {
        .shape = :Expr :Apply { .f, .args },
        .ty = f_type.result,
    }
);

const parse_context_obj = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    {
        .shape = :Place :ContextObject,
        .ty = :ContextObject,
    }
);

const parse_let_context = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
    root :: Ast.Group,
) -> ParsedExpr => (
    let value = root |> AstHelpers.expect_single_child(:None);
    let value = parse_expr(:Some :Ref :ContextObject, value);
    {
        .shape = :Expr :LetContextRef value,
        .ty = :Unit,
    }
);

const parsers = (
    let mut map = OrdMap.new();
    &mut map |> OrdMap.add("instantiate", parse_instantiate_expr);
    &mut map |> OrdMap.add("variant", parse_variant);
    &mut map |> OrdMap.add("==", parse_equals);
    &mut map |> OrdMap.add("type_info", parse_type_info);
    &mut map |> OrdMap.add("deref", parse_deref);
    &mut map |> OrdMap.add("ref", parse_ref);
    &mut map |> OrdMap.add("record", parse_record);
    &mut map |> OrdMap.add("list", parse_list);
    &mut map |> OrdMap.add("fn", parse_fn);
    &mut map |> OrdMap.add("field", parse_field);
    &mut map |> OrdMap.add("assign", parse_assign);
    &mut map |> OrdMap.add("uninitialized", parse_uninitialized);
    &mut map |> OrdMap.add("type ascribe", parse_type_ascribe);
    &mut map |> OrdMap.add("unwind", parse_unwind);
    &mut map |> OrdMap.add("unwindable", parse_unwindable);
    &mut map |> OrdMap.add("loop", parse_loop);
    &mut map |> OrdMap.add("if", parse_if);
    &mut map |> OrdMap.add("if_without_else", parse_if);
    &mut map |> OrdMap.add("let", parse_let);
    &mut map |> OrdMap.add("let context", parse_let_context);
    &mut map |> OrdMap.add("native", parse_native);
    &mut map |> OrdMap.add("stmt", parse_stmt);
    &mut map |> OrdMap.add("scope", parse_scope);
    &mut map |> OrdMap.add("inject_context", parse_inject_context);
    &mut map |> OrdMap.add("current_context", parse_current_context);
    &mut map |> OrdMap.add("then", parse_then);
    &mut map |> OrdMap.add("apply", parse_apply);
    &mut map |> OrdMap.add("defer", parse_defer);
    &mut map |> OrdMap.add("context_obj", parse_context_obj);
    &mut map |> OrdMap.add("true", (...) => { .shape = :Expr :Literal :Bool true, .ty = :Bool });
    &mut map |> OrdMap.add("false", (...) => { .shape = :Expr :Literal :Bool false, .ty = :Bool });
    &mut map |> OrdMap.add("delimited_continuation", parse_delimited_continuation);
    &mut map |> OrdMap.add("capture_continuation", parse_capture_continuation);
    map
);

const parse_expr_impl = (
    expected_ty :: Option.t[Ir.Type],
    ast :: Ast.t,
) -> ParsedExpr => (
    let { .shape, .ty } = with_return (
        match ast.shape with (
            | :Empty => return {
                .shape = :Expr :Unit,
                .ty = :Unit,
            }
            | :Rule { .rule, .root } => (
                if &parsers |> OrdMap.get(rule.name) is :Some parser then (
                    return parser^(expected_ty, ast, root);
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
                        let literal = match (@current Compiler).resolve_type_aliases(ty) with (
                            | :Int => (
                                # TODO check that its within range
                                :Int raw
                            )
                            | :UInt => (
                                # TODO check that its within range
                                :Int raw
                            )
                            | :IntSpecific {
                                .signed,
                                .bits,
                            } => (
                                # TODO check that its within range
                                :Int raw
                            )
                            | :Float32 => :Float raw
                            | :Float64 => :Float raw
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
