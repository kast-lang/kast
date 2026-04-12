use (import "../../span.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;

module:

const TypeDef = newtype (
    | :Opaque
    | :Enum {
        .variants :: OrdSet.t[String],
    }
    | :Union {
        .variants :: OrdMap.t[String, Type],
    }
    | :Struct {
        .fields :: OrdMap.t[String, Type],
    }
    | :Alias Type
);

const FnType = newtype {
    .args :: ArrayList.t[Type],
    .result :: Type,
};

const Type = newtype (
    | :Any
    | :Ref Type
    | :Unit
    | :Int32
    | :Int64
    | :Float64
    | :Bool
    | :Char
    | :Named String
    | :Fn FnType
    | :Native String
);

# TODO derive
const compare_type = (
    a :: &Type,
    b :: &Type,
) -> std.cmp.Ordering => (
    match { a^, b^ } with (
        | { :Any, :Any } => :Equal
        | { :Any, _ } => :Less
        | { _, :Any } => :Greater
        | { :Unit, :Unit } => :Equal
        | { :Unit, _ } => :Less
        | { _, :Unit } => :Greater
        | { :Bool, :Bool } => :Equal
        | { :Bool, _ } => :Less
        | { _, :Bool } => :Greater
        | { :Int32, :Int32 } => :Equal
        | { :Int32, _ } => :Less
        | { _, :Int32 } => :Greater
        | { :Int64, :Int64 } => :Equal
        | { :Int64, _ } => :Less
        | { _, :Int64 } => :Greater
        | { :Float64, :Float64 } => :Equal
        | { :Float64, _ } => :Less
        | { _, :Float64 } => :Greater
        | { :Char, :Char } => :Equal
        | { :Char, _ } => :Less
        | { _, :Char } => :Greater
        | { :Named a, :Named b } => std.cmp.default_compare[String](a, b)
        | { :Named _, _ } => :Less
        | { _, :Named _ } => :Greater
        | { :Ref ref a, :Ref ref b } => compare_type(a, b)
        | { :Ref _, _ } => :Less
        | { _, :Ref _ } => :Greater
        | { :Native a, :Native b } => std.cmp.default_compare[String](a, b)
        | { :Native _, _ } => :Less
        | { _, :Native _ } => :Greater
        | { :Fn ref ty, :Fn ref ty } => panic("TODO compare fn types")
        | { :Fn _, _ } => :Less
        | { _, :Fn _ } => :Greater
    )
);

const NativeExprPart = newtype (
    | :Raw String
    | :Interpolated Expr
);

const NativeExpr = newtype {
    .parts :: ArrayList.t[NativeExprPart],
};

const Literal = newtype (
    | :Bool Bool
    | :Int32 Int32
    | :Int64 Int64
    | :Float64 Float64
    | :Char Char
    | :String String
);

const ExprShape = newtype (
    | :Unit
    | :Uninitialized
    | :Claim PlaceExpr
    | :Ref PlaceExpr
    | :Native NativeExpr
    | :Literal Literal
    | :Variant String
    | :Stmt Expr
    | :Let {
        .name :: String,
        .value :: Expr,
    }
    | :Assign {
        .assignee :: PlaceExpr,
        .value :: Expr,
    }
    | :Fn FnDef
    | :Then ArrayList.t[Expr]
    | :Scope Expr
    | :If {
        .cond :: Expr,
        .then_case :: Expr,
        .else_case :: Option.t[Expr],
    }
    | :Apply {
        .f :: Expr,
        .args :: ArrayList.t[Expr],
    }
    | :InjectContext {
        .name :: String,
        .value :: Expr,
    }
    | :Record ArrayList.t[Field]
    | :EnumIs {
        .enum :: Expr,
        .variant :: String,
    }
);

const Field = newtype {
    .name :: String,
    .value :: Expr,
};

const Expr = newtype {
    .shape :: ExprShape,
    .ty :: Type,
    .span :: Span,
};

const PlaceExprShape = newtype (
    | :Ident String
    | :Field {
        .obj :: PlaceExpr,
        .field :: String,
    }
    | :Index {
        .list :: PlaceExpr,
        .index :: Expr,
    }
    | :CurrentContext String
    | :Deref Expr
    | :Temp Expr
);

const PlaceExpr = newtype {
    .shape :: PlaceExprShape,
    .ty :: Type,
    .span :: Span,
};

const FnArg = newtype {
    .name :: String,
    .ty :: Type,
};

const FnDef = newtype {
    .args :: ArrayList.t[FnArg],
    .result_ty :: Type,
    .body :: Expr,
    .span :: Span,
};

const Program = newtype {
    .types :: OrdMap.t[String, TypeDef],
    .contexts :: OrdMap.t[String, Type],
    .consts :: OrdMap.t[String, Expr],
    .consts_order :: ArrayList.t[String],
    .fns :: OrdMap.t[String, FnDef],
};