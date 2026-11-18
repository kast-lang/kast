module:

use (import "../_common.ks").*;
use (import "../scope.ks").*;

const CompileSpecific = [K] type (
    (&Ast.t, .root :: &Ast.Group, .expected_ty :: Option.t[Ty]) -> K
);

const CoreSyntax = newtype {
    .name :: &str,
    .compile :: {
        .any_expr :: CompileSpecific[AnyExpr],
        .expr :: CompileSpecific[Expr],
        .place_expr :: CompileSpecific[PlaceExpr],
        .ty_expr :: CompileSpecific[TyExpr],
        .assignee :: CompileSpecific[Assignee],
        .pattern :: CompileSpecific[Pattern],
    },
};

const impl_compile = (Trait :: std.Ast) -> std.Ast => `(
    {
        .any_expr = (AnyExpr as $Trait).compile,
        .expr = (Expr as $Trait).compile,
        .place_expr = (PlaceExpr as $Trait).compile,
        .ty_expr = (TyExpr as $Trait).compile,
        .assignee = (Assignee as $Trait).compile,
        .pattern = (Pattern as $Trait).compile,
    }
);

const impl_any_expr_syntax = (
    name :: &str,
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be a pattern");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be an assignee");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };

    (:: CoreSyntax) {
        .name,
        .compile = impl_compile!(CompileTrait),
    }
);

const impl_pattern_syntax = (
    name :: &str,
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be an expr");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be an expr");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be a place expr");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be a type expr");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be an assignee");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };

    (:: CoreSyntax) {
        .name,
        .compile = impl_compile!(CompileTrait),
    }
);

const impl_assignee_syntax = (
    name :: &str,
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be an expr");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be an expr");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be a place expr");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be a type expr");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be a pattern");
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
        .compile = impl_compile!(CompileTrait),
    }
);

const impl_type_expr_syntax = (
    name :: &str,
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be a pattern");
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
                    locked_output.write_str(name);
                    locked_output.write_str(" can't be an assignee");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        ),
    };

    (:: CoreSyntax) {
        .name,
        .compile = {
            .any_expr = (ast, .root, .expected_ty) => (AnyExpr as CompileTrait).compile(ast, .root),
            .expr = (ast, .root, .expected_ty) => (Expr as CompileTrait).compile(ast, .root),
            .place_expr = (ast, .root, .expected_ty) => (PlaceExpr as CompileTrait).compile(ast, .root),
            .ty_expr = (ast, .root, .expected_ty) => (TyExpr as CompileTrait).compile(ast, .root),
            .assignee = (ast, .root, .expected_ty) => (Assignee as CompileTrait).compile(ast, .root),
            .pattern = (ast, .root, .expected_ty) => (Pattern as CompileTrait).compile(ast, .root),
        },
    }
);
