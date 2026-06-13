const ExprShape = newtype (
    | :Unit
    | :Read PlaceExpr
    | :Type TyExpr
    | :Const Place
    | :Assign {
        .assignee :: Assignee,
        .value :: PlaceExpr,
    }
    | :Native String
    | :Stmt Expr
    | :Then ArrayList.t[Expr]
    | :Apply { .f :: Expr, .args :: ArrayList.t[Expr] }
    | :Scope Expr
    | :Quote QuoteExpr.t
);

const Expr = newtype {
    .shape :: ExprShape,
    .ty :: Ty,
    .span :: Span,
};

const QuoteExpr = (
    module:

    use (
        include_ast MakeAstModule(
            (.t, .t_span, .Shape) => {
                .t_def = `(
                    const Shape = newtype (
                        | :Unquote Expr
                        | :Construct $Shape.t
                    );
                    newtype {
                        .shape :: Shape,
                        .ignored_tokens_before :: ArrayList.t[Token.t],
                        .span :: Span,
                    }
                ),
                .t_span_def = `(
                    (expr :: &$t) -> Span => expr^.span
                ),
                .print_def = `(
                    (expr :: &$t) => (
                        match expr^.shape with (
                            | :Unquote ref expr => panic("TODO print expr")
                            | :Construct ref shape => (
                                $Shape.print(shape);
                            )
                        )
                    )
                ),
            }
        )
    ).*;
);

const PlaceExprShape = newtype (
    | :Temp Expr
    | :Binding Binding
);

const PlaceExpr = newtype {
    .shape :: PlaceExprShape,
    .ty :: Ty,
    .span :: Span,
};

const TyExprShape = newtype (
    | :Const Ty
    | :Expr Expr
    | :Fn {
        .args :: ArrayList.t[TyExpr],
        .result :: TyExpr,
    }
);

const TyExpr = newtype {
    .shape :: TyExprShape,
    .span :: Span,
};
