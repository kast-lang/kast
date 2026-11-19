use (import "./_common.ks").*;
use (import "./type_check.ks").*;

const super = @current_scope;

module:

const Compiler = (
    module:

    use (import "./scope.ks").*;
    use (import "./core_syntax/_lib.ks").*;
    use (import "./custom_syntax.ks").*;

    const State = newtype {
        .core_syntax :: CoreSyntax.Map,
        .custom_syntax :: OrdMap.t[RcStr, CustomSyntax],
    };

    const StateContext = @context type (&mut State);
    const Context = CompilerContext;
    const InMut = super.InMut;

    const init = () -> {
        .compiler :: CompilerContextT,
        .state :: State,
        .scope :: Scope.t,
    } => {
        .compiler = {
            .inject_assignee_bindings = Scope.inject_assignee_bindings,
            .compile = {
                .any_expr = compile[AnyExpr],
                .expr = compile[Expr],
                .place_expr = compile[PlaceExpr],
                .ty_expr = compile[TyExpr],
                .assignee = compile[Assignee],
                .pattern = compile[Pattern],
            },
            .impl_syntax,
        },
        .state = {
            .core_syntax = CoreSyntax.init(),
            .custom_syntax = OrdMap.new(),
        },
        .scope = Scope.new(.parent = :None),
    };

    const or_default_type = (ty :: Option.t[Ty], default :: Ty) -> { Ty, &TyShape } => (
        let ty = ty |> Option.unwrap_or(default);
        let ty_shape = with_return (
            &ty |> Ty.ensure_inferred_or(() => (
                type_check_impl(
                    .actual = default,
                    .expected = ty,
                );
                return &default
                    |> Ty.ensure_inferred_or(() => panic("impossible"))
            ))
        );
        { ty, ty_shape }
    );

    const Compilable = [Self] newtype {
        .set_span :: (&mut Self, Span) -> (),
        .unit :: (.span :: Span) -> Self,
        .number :: (&Token.NumberToken, .span :: Span, .expected_ty :: Option.t[Ty]) -> Self,
        .ident :: (&RcStr, .span :: Span, .expected_ty :: Option.t[Ty]) -> Self,
        .string :: (&Token.StringToken, .span :: Span, .expected_ty :: Option.t[Ty]) -> Self,
        .core_syntax :: (&CoreSyntax.t, &Ast.t, .root :: &Ast.Group, .expected_ty :: Option.t[Ty]) -> Self,
    };

    impl AnyExpr as Compilable = {
        .set_span = (...) => (),
        .unit = (.span) => { .shape = :Expr :Unit, .ty = Ty.UNIT },
        .number = (&{ .raw = ref raw }, .span, .expected_ty) => (
            let { ty, ty_shape } = expected_ty |> or_default_type(Ty.INT);
            let value_shape :: ValueShape = match ty_shape^ with (
                | :Int => :Int parse(raw |> RcStr.as_str)
                | _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .span,
                        .message = () => (
                            locked_output.write_str("Number literal can not be ");
                            Ty.print(&ty);
                        ),
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            );
            let place = Place.init(Rc.new({ .shape = value_shape, .ty }));
            {
                .shape = :Expr :Const place,
                .ty,
            }
        ),
        .string = (&{ .contents, ... }, .span, .expected_ty) => (
            let { ty, ty_shape } = expected_ty |> or_default_type(Ty.STRING);
            let value_shape :: ValueShape = match ty_shape^ with (
                | :String => :String contents
                | _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .span,
                        .message = () => (
                            locked_output.write_str("String literal can not be ");
                            Ty.print(&ty);
                        ),
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            );
            let place = Place.init(Rc.new({ .shape = value_shape, .ty }));
            {
                .shape = :Expr :Const place,
                .ty,
            }
        ),
        .ident = (name, .span, .expected_ty) => with_return (
            let local = Scope.lookup(name, .span);
            let result = match local^ with (
                | :Binding binding => {
                    .shape = :Place :Binding binding,
                    .ty = binding^.ty,
                }
                | :Const place => {
                    .shape = :Expr :Const place,
                    .ty = place^.ty,
                }
            );
            if expected_ty is :Some expected_ty then (
                type_check(
                    .actual = result.ty,
                    .expected = expected_ty,
                    .span,
                );
            );
            result
        ),
        .core_syntax = (core_syntax, ...args) => (
            core_syntax^.compile.any_expr(...args)
        ),
    };

    impl Expr as Compilable = {
        .set_span = (self, span) => (
            self^^.span = span;
        ),
        .unit = (.span) => any_expr_to_expr(
            (AnyExpr as Compilable).unit(.span),
            .span,
        ),
        .number = (...args) => any_expr_to_expr(
            (AnyExpr as Compilable).number(...args),
            .span = args.span,
        ),
        .string = (...args) => any_expr_to_expr(
            (AnyExpr as Compilable).string(...args),
            .span = args.span,
        ),
        .ident = (...args) => any_expr_to_expr(
            (AnyExpr as Compilable).ident(...args),
            .span = args.span,
        ),
        .core_syntax = (core_syntax, ...args) => (
            core_syntax^.compile.expr(...args)
        ),
    };

    impl PlaceExpr as Compilable = {
        .set_span = (self, span) => (
            self^^.span = span;
        ),
        .unit = (.span) => any_expr_to_place_expr(
            (AnyExpr as Compilable).unit(.span),
            .span,
        ),
        .number = (...args) => any_expr_to_place_expr(
            (AnyExpr as Compilable).number(...args),
            .span = args.span,
        ),
        .string = (...args) => any_expr_to_place_expr(
            (AnyExpr as Compilable).string(...args),
            .span = args.span,
        ),
        .ident = (...args) => any_expr_to_place_expr(
            (AnyExpr as Compilable).ident(...args),
            .span = args.span,
        ),
        .core_syntax = (core_syntax, ...args) => (
            core_syntax^.compile.place_expr(...args)
        ),
    };

    impl TyExpr as Compilable = {
        .set_span = (self, span) => (
            self^^.span = span;
        ),
        .unit = (.span) => Rc.new({
            .shape = :Const Ty.UNIT,
            .span,
        }),
        .number = (.span, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span,
                .message = () => (
                    locked_output.write_str("Expected a type, got number literal");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
        .string = (...args) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = args.span,
                .message = () => (
                    locked_output.write_str("Expected a type, got string literal");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
        .ident = (...args) => (
            let expr = (Expr as Compilable).ident(...args);
            let span = args.span;
            type_check(.actual = expr^.ty, .expected = Ty.TYPE, .span);
            Rc.new({ .shape = :Expr expr, .span })
        ),
        .core_syntax = (core_syntax, ...args) => (
            core_syntax^.compile.ty_expr(...args)
        ),
    };

    impl Assignee as Compilable = {
        .set_span = (self, span) => (
            self^^.span = span;
        ),
        .unit = (.span) => panic("TODO unit assignee"),
        .number = (..., .span) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span,
                .message = () => (
                    locked_output.write_str("Can't assign to a number literal");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
        .string = (..., .span) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span,
                .message = () => (
                    locked_output.write_str("Can't assign to a string literal");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
        .ident = (name, .span, .expected_ty) => (
            let local = Scope.lookup(name, .span);
            match local^ with (
                | :Binding binding => (
                    if not binding^.mutable then (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Compiler,
                            .span,
                            .message = () => (
                                locked_output.write(&binding^.name);
                                locked_output.write_str(" is not mutable");
                            ),
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    );
                    Rc.new({
                        .shape = :Binding binding,
                        .ty = binding^.ty,
                        .span,
                    })
                )
                | :Const _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .span,
                        .message = () => (
                            locked_output.write_str("Can't assign to a constant");
                        ),
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            )
        ),
        .core_syntax = (core_syntax, ...args) => (
            core_syntax^.compile.assignee(...args)
        ),
    };

    impl Pattern as Compilable = {
        .set_span = (self, span) => (
            self^^.span = span;
        ),
        .unit = (.span) => panic("TODO unit pattern"),
        .number = (...) => panic("TODO number pattern"),
        .string = (...) => panic("TODO string pattern"),
        .ident = (name, .span, .expected_ty) => (
            let ty = match expected_ty with (
                | :Some ty => ty
                | :None => with_return (
                    return Ty.new_not_inferred();
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .span,
                        .message = () => (
                            locked_output.write_str("Failed to infer binding type");
                        ),
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            );
            let binding :: Binding = Rc.new({
                .id = Id.gen(),
                .name = name^,
                .ty,
                .mutable = @current InMut,
            });
            Rc.new({
                .shape = :Binding binding,
                .ty,
                .span,
            })
        ),
        .core_syntax = (core_syntax, ...args) => (
            core_syntax^.compile.pattern(...args)
        ),
    };

    const compile = [K] (ast :: &Ast.t, .expected_ty :: Option.t[Ty]) -> K => (
        let state = @current StateContext;
        let span = ast^.span;
        match ast^.shape^ with (
            | :Empty => (K as Compilable).unit(.span)
            | :Token ref token => match token^.shape^ with (
                | :Ident { .name = ref name, ... } => (K as Compilable).ident(name, .span, .expected_ty)
                | :Number ref number => (K as Compilable).number(number, .span, .expected_ty)
                | :String ref s => (K as Compilable).string(s, .span, .expected_ty)
            )
            | :Rule {
                .rule = ref rule,
                .root = ref root,
            } => match &rule^^.name |> RcStr.as_str |> String.strip_prefix(.prefix = "core:") with (
                | :Some core_syntax_name => match &state^.core_syntax |> OrdMap.get(&core_syntax_name) with (
                    | :None => (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Compiler,
                            .span,
                            .message = () => (
                                locked_output.write_str("Core syntax not found: ");
                                locked_output.write_str(&String.escape(core_syntax_name) |> as_str);
                            ),
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    )
                    | :Some core_syntax => (
                        (K as Compilable).core_syntax(core_syntax, ast, .root, .expected_ty)
                    )
                )
                | :None => match &state^.custom_syntax |> OrdMap.get(&rule^^.name) with (
                    | :Some custom_syntax => (
                        let mut compiled = compile[K](&CustomSyntax.expand(custom_syntax, root), .expected_ty);
                        (K as Compilable).set_span(&mut compiled, span);
                        compiled
                    )
                    | :None => (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Compiler,
                            .span,
                            .message = () => (
                                locked_output.write_str("no impl for syntax ");
                                locked_output.write_escaped(&rule^^.name);
                            ),
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    )
                )
            )
        )
    );

    const impl_syntax = (
        .rule :: &Rc.t[SyntaxRule.t],
        .pattern_root :: &Ast.Group,
        .bindings :: OrdMap.t[RcStr, Binding],
        .@"impl" :: Expr,
    ) => (
        let custom_syntax :: CustomSyntax = {
            .pattern_root = pattern_root^,
            .bindings,
            .@"impl",
        };
        &mut (@current StateContext)^.custom_syntax
            |> OrdMap.add(rule^^.name, custom_syntax);
    );
);
