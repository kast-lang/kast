const ExprShape = newtype (
    | :Unit
    | :Read PlaceExpr
    | :Const Place
    | :Assign {
        .assignee :: Assignee,
        .value :: PlaceExpr,
    }
);

const Expr = newtype {
    .shape :: ExprShape,
    .ty :: Ty,
    .span :: Span,
};

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
    | :Expr Expr
);

const TyExpr = newtype {
    .shape :: TyExprShape,
    .span :: Span,
};
