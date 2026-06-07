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
        .custom_syntax :: OrdMap.t[String, CustomSyntax],
    };

    const StateContext = @context State;
    const Context = CompilerContext;
    const InMut = super.InMut;

    const init = () -> {
        .compiler :: CompilerContextT,
        .state :: State,
        .scope :: Scope.t,
    } => {
        .compiler = {
            .inject_assignee_bindings = Scope.inject_assignee_bindings,
            .compile,
            .impl_syntax,
        },
        .state = {
            .core_syntax = CoreSyntax.init(),
            .custom_syntax = OrdMap.new(),
        },
        .scope = Scope.new(.parent = :None),
    };

    const Compilable = [Self] newtype {
        .unit :: (.span :: Span) -> Self,
        .number :: (Token.NumberToken, .span :: Span, .expected_ty :: Option.t[Ty]) -> Self,
        .ident :: (String, .span :: Span, .expected_ty :: Option.t[Ty]) -> Self,
        .string :: (Token.StringToken, .span :: Span, .expected_ty :: Option.t[Ty]) -> Self,
    };

    impl AnyExpr as Compilable = {
        .unit = (.span) => { .shape = :Expr :Unit, .ty = Ty.UNIT },
        .number = ({ .raw }, .span, .expected_ty) => (
            let ty = expected_ty |> Option.unwrap_or(Ty.INT);
            let value_shape :: ValueShape = match ty.shape with (
                | :Int => :Int parse(raw)
                | _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .span,
                        .message = () => (
                            let output = @current Output;
                            output.write("Number literal can not be ");
                            Ty.print(&ty);
                        ),
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            );
            let place = Place.init({ .shape = value_shape, .ty });
            {
                .shape = :Expr :Const place,
                .ty,
            }
        ),
        .string = ({ .contents, ... }, .span, .expected_ty) => (
            let ty = expected_ty |> Option.unwrap_or(Ty.STRING);
            let value_shape :: ValueShape = match ty.shape with (
                | :String => :String contents
                | _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .span,
                        .message = () => (
                            let output = @current Output;
                            output.write("String literal can not be ");
                            Ty.print(&ty);
                        ),
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            );
            let place = Place.init({ .shape = value_shape, .ty });
            {
                .shape = :Expr :Const place,
                .ty,
            }
        ),
        .ident = (name, .span, .expected_ty) => with_return (
            let local = Scope.lookup(name, .span);
            match local^ with (
                | :Binding binding => {
                    .shape = :Place :Binding binding,
                    .ty = binding.ty,
                }
                | :Const place => {
                    .shape = :Expr :Const place,
                    .ty = place.ty,
                }
            )
        ),
    };

    impl Expr as Compilable = {
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
    };

    impl PlaceExpr as Compilable = {
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
    };

    impl TyExpr as Compilable = {
        .unit = (.span) => {
            .shape = :Const Ty.UNIT,
            .span,
        },
        .number = (.span, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span,
                .message = () => (
                    let output = @current Output;
                    output.write("Expected a type, got number literal");
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
                    let output = @current Output;
                    output.write("Expected a type, got string literal");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
        .ident = (...args) => (
            let expr = (Expr as Compilable).ident(...args);
            let span = args.span;
            type_check(.actual = expr.ty, .expected = Ty.TYPE, .span);
            { .shape = :Expr expr, .span }
        )
    };

    impl Assignee as Compilable = {
        .unit = (.span) => panic("TODO unit assignee"),
        .number = (..., .span) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span,
                .message = () => (
                    let output = @current Output;
                    output.write("Can't assign to a number literal");
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
                    let output = @current Output;
                    output.write("Can't assign to a string literal");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
        .ident = (name, .span, .expected_ty) => (
            let local = Scope.lookup(name, .span);
            match local^ with (
                | :Binding binding => (
                    if not binding.mutable then (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Compiler,
                            .span,
                            .message = () => (
                                let output = @current Output;
                                output.write(binding.name);
                                output.write(" is not mutable");
                            ),
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    );
                    {
                        .shape = :Binding binding,
                        .ty = binding.ty,
                        .span,
                    }
                )
                | :Const _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .span,
                        .message = () => (
                            let output = @current Output;
                            output.write("Can't assign to a constant");
                        ),
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            )
        ),
    };

    impl Pattern as Compilable = {
        .unit = (.span) => panic("TODO unit pattern"),
        .number = (...) => panic("TODO number pattern"),
        .string = (...) => panic("TODO string pattern"),
        .ident = (name, .span, .expected_ty) => (
            let ty = match expected_ty with (
                | :Some ty => ty
                | :None => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .span,
                        .message = () => (
                            let output = @current Output;
                            output.write("Failed to infer binding type");
                        ),
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            );
            let binding :: Binding = {
                .id = Id.gen(),
                .name,
                .ty,
                .mutable = @current InMut,
            };
            {
                .shape = :Binding binding,
                .ty,
                .span,
            }
        )
    };

    const compile = [K] (ast :: &Ast.t, .expected_ty :: Option.t[Ty]) -> K => (
        let state = @current StateContext;
        let span = ast^.span;
        match ast^.shape with (
            | :Empty => (K as Compilable).unit(.span)
            | :Token { .shape = token_shape, .span = _ } => match token_shape with (
                | :Ident { .name, ... } => (K as Compilable).ident(name, .span, .expected_ty)
                | :Number number => (K as Compilable).number(number, .span, .expected_ty)
                | :String s => (K as Compilable).string(s, .span, .expected_ty)
            )
            | :Rule {
                .rule,
                .root = ref root,
            } => match rule.name |> String.strip_prefix(.prefix = "core:") with (
                | :Some core_syntax_name => match &state.core_syntax |> OrdMap.get(core_syntax_name) with (
                    | :None => (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Compiler,
                            .span,
                            .message = () => (
                                let output = @current Output;
                                output.write("Core syntax not found: ");
                                output.write(String.escape(core_syntax_name));
                            ),
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    )
                    | :Some core_syntax => (
                        core_syntax^.compile[K](ast, .root, .expected_ty)
                    )
                )
                | :None => match &state.custom_syntax |> OrdMap.get(rule.name) with (
                    | :Some custom_syntax => (
                        compile[K](&CustomSyntax.expand(custom_syntax, root), .expected_ty)
                    )
                    | :None => (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Compiler,
                            .span,
                            .message = () => (
                                let output = @current Output;
                                output.write("no impl for syntax ");
                                output.write(String.escape(rule.name));
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
        .rule :: SyntaxRule.t,
        .pattern_root :: Ast.Group,
        .bindings :: OrdMap.t[String, Binding],
        .@"impl" :: Expr,
    ) => (
        let custom_syntax :: CustomSyntax = {
            .pattern_root,
            .bindings,
            .@"impl",
        };
        &mut (@current StateContext).custom_syntax
            |> OrdMap.add(rule.name, custom_syntax);
    );
);
