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
