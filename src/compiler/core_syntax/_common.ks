module:

use (import "../_common.ks").*;
use (import "../scope.ks").*;

const CoreSyntax = newtype {
    .name :: String,
    .compile :: [K] (&Ast.t, .root :: &Ast.Group, .expected_ty :: Option.t[Ty]) -> K,
};

const impl_any_expr_syntax = (
    name :: String,
    compile_fn :: (
        &Ast.t,
        .root :: &Ast.Group,
        .expected_ty :: Option.t[Ty],
    ) -> AnyExpr,
) -> std.Ast => `(
    const CompileTrait = [Self] newtype {
        .compile :: (
            &Ast.t,
            .root :: &Ast.Group,
            .expected_ty :: Option.t[Ty],
        ) -> Self,
    };

    impl AnyExpr as CompileTrait = {
        .compile = compile_fn,
    };
    impl Expr as CompileTrait = {
        .compile = (...args) => any_expr_to_expr(
            (AnyExpr as CompileTrait).compile(...args),
            .span = args.0^.span,
        ),
    };
    impl PlaceExpr as CompileTrait = {
        .compile = (...args) => any_expr_to_place_expr(
            (AnyExpr as CompileTrait).compile(...args),
            .span = args.0^.span,
        ),
    };
    impl TyExpr as CompileTrait = {
        .compile = (...args) => any_expr_to_type_expr(
            (AnyExpr as CompileTrait).compile(...args),
            .span = args.0^.span,
        ),
    };
    impl Pattern as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be a pattern");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl Assignee as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be an assignee");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };

    (:: CoreSyntax) {
        .name,
        .compile = [K] (
            ast :: &Ast.t,
            .root :: &Ast.Group,
            .expected_ty :: Option.t[Ty],
        ) -> K => (
            (K as CompileTrait).compile(ast, .root, .expected_ty)
        ),
    }
);

const impl_pattern_syntax = (
    name :: String,
    compile_fn :: (
        &Ast.t,
        .root :: &Ast.Group,
        .expected_ty :: Option.t[Ty],
    ) -> Pattern,
) -> std.Ast => `(
    const CompileTrait = [Self] newtype {
        .compile :: (
            &Ast.t,
            .root :: &Ast.Group,
            .expected_ty :: Option.t[Ty],
        ) -> Self,
    };

    impl AnyExpr as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be an expr");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl Expr as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be an expr");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl PlaceExpr as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be a place expr");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl TyExpr as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be a type expr");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl Pattern as CompileTrait = {
        .compile = compile_fn,
    };
    impl Assignee as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be an assignee");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };

    (:: CoreSyntax) {
        .name,
        .compile = [K] (
            ast :: &Ast.t,
            .root :: &Ast.Group,
            .expected_ty :: Option.t[Ty],
        ) -> K => (
            (K as CompileTrait).compile(ast, .root, .expected_ty)
        ),
    }
);

const impl_assignee_syntax = (
    name :: String,
    compile_fn :: (
        &Ast.t,
        .root :: &Ast.Group,
        .expected_ty :: Option.t[Ty],
    ) -> Assignee,
) -> std.Ast => `(
    const CompileTrait = [Self] newtype {
        .compile :: (
            &Ast.t,
            .root :: &Ast.Group,
            .expected_ty :: Option.t[Ty],
        ) -> Self,
    };

    impl AnyExpr as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be an expr");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl Expr as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be an expr");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl TyExpr as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be a type expr");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl Pattern as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be a pattern");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl Assignee as CompileTrait = {
        .compile = compile_fn,
    };

    (:: CoreSyntax) {
        .name,
        .compile = [K] (
            ast :: &Ast.t,
            .root :: &Ast.Group,
            .expected_ty :: Option.t[Ty],
        ) -> K => (
            (K as CompileTrait).compile(ast, .root, .expected_ty)
        ),
    }
);

const impl_type_expr_syntax = (
    name :: String,
    compile_fn :: (
        &Ast.t,
        .root :: &Ast.Group,
    ) -> TyExpr,
) -> std.Ast => `(
    const CompileTrait = [Self] newtype {
        .compile :: (
            &Ast.t,
            .root :: &Ast.Group,
        ) -> Self,
    };

    impl AnyExpr as CompileTrait = {
        .compile = (...args) => {
            .shape = :Expr :Type (TyExpr as CompileTrait).compile(...args),
            .ty = Ty.TYPE,
        },
    };
    impl Expr as CompileTrait = {
        .compile = (...args) => any_expr_to_expr(
            (AnyExpr as CompileTrait).compile(...args),
            .span = args.0^.span,
        ),
    };
    impl PlaceExpr as CompileTrait = {
        .compile = (...args) => any_expr_to_place_expr(
            (AnyExpr as CompileTrait).compile(...args),
            .span = args.0^.span,
        ),
    };
    impl TyExpr as CompileTrait = {
        .compile = compile_fn,
    };
    impl Pattern as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be a pattern");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };
    impl Assignee as CompileTrait = {
        .compile = (ast, ...) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write(name);
                    output.write(" can't be an assignee");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };

    (:: CoreSyntax) {
        .name,
        .compile = [K] (
            ast :: &Ast.t,
            .root :: &Ast.Group,
            .expected_ty :: Option.t[Ty],
        ) -> K => (
            (K as CompileTrait).compile(ast, .root)
        ),
    }
);
