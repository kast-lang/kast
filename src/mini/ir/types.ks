use (import "../../span.ks").*;
use (import "../../rc_str.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;
use std.rc.Rc;

module:

const TypeDefShape = newtype (
    | :Opaque
    | :Enum {
        .variants :: OrdSet.t[RcStr],
    }
    | :Union {
        .variants :: OrdMap.t[RcStr, Type],
    }
    | :Struct {
        .fields :: OrdMap.t[RcStr, Type],
    }
    | :Alias Type
);

const TypeDef = newtype {
    .shape :: TypeDefShape,
    .native :: Bool,
};

const FnType = newtype {
    .is_closure :: Bool,
    .call_convention :: Option.t[RcStr],
    .args :: ArrayList.t[Type],
    .result :: Type,
};

const TypeShape = newtype (
    | :Any
    | :Ref Type
    | :Unit
    | :Int
    | :UInt
    | :IntSpecific {
        .signed :: Bool,
        .bits :: Int32,
    }
    | :Float32
    | :Float64
    | :Bool
    | :Char
    | :Named RcStr
    | :Fn FnType
    | :Native RcStr
    | :Array {
        .repr :: Type,
        .element_ty :: Type,
    }
    | :UnwindToken {
        .repr :: Type,
        .result_ty :: Type,
    }
    | :DelimitedContinuationToken {
        .repr :: Type,
        .result_ty :: Type,
    }
    | :ContextObject
);

const Type = newtype {
    .shape :: Rc.t[TypeShape],
    .alias_name :: Option.t[RcStr],
};

const type_repr = (ty :: &Type) -> &Type => (
    match ty^.shape^ with (
        | :UnwindToken { .repr = ref repr, ... } => repr
        | :DelimitedContinuationToken { .repr = ref repr, ... } => repr
        | :Array { .repr = ref repr, ... } => repr
        | _ => ty
    )
);

impl Type as std.cmp.Ord = {
    # TODO derive
    .compare = (a, b) => with_return (
        match ({ &a^.shape^, &b^.shape^ } :: { _, _ }) with (
            | { &(:Any), &(:Any) } => :Equal
            | { &(:Any), _ } => :Less
            | { _, &(:Any) } => :Greater
            | { &(:Unit), &(:Unit) } => :Equal
            | { &(:Unit), _ } => :Less
            | { _, &(:Unit) } => :Greater
            | { &(:Bool), &(:Bool) } => :Equal
            | { &(:Bool), _ } => :Less
            | { _, &(:Bool) } => :Greater
            | { &(:Int), &(:Int) } => :Equal
            | { &(:Int), _ } => :Less
            | { _, &(:Int) } => :Greater
            | { &(:UInt), &(:UInt) } => :Equal
            | { &(:UInt), _ } => :Less
            | { _, &(:UInt) } => :Greater
            | { &(:IntSpecific ref a), &(:IntSpecific ref b) } => (
                match std.cmp.default_compare(&a^.signed, &b^.signed) with (
                    | :Equal => ()
                    | other => return other
                );
                std.cmp.default_compare(&a^.bits, &b^.bits)
            )
            | { &(:IntSpecific _), _ } => :Less
            | { _, &(:IntSpecific _) } => :Greater
            | { &(:Float32), &(:Float32) } => :Equal
            | { &(:Float32), _ } => :Less
            | { _, &(:Float32) } => :Greater
            | { &(:Float64), &(:Float64) } => :Equal
            | { &(:Float64), _ } => :Less
            | { _, &(:Float64) } => :Greater
            | { &(:Char), &(:Char) } => :Equal
            | { &(:Char), _ } => :Less
            | { _, &(:Char) } => :Greater
            | { &(:Named ref a), &(:Named ref b) } => std.cmp.default_compare(a, b)
            | { &(:Named _), _ } => :Less
            | { _, &(:Named _) } => :Greater
            | { &(:Ref ref a), &(:Ref ref b) } => std.cmp.default_compare(a, b)
            | { &(:Ref _), _ } => :Less
            | { _, &(:Ref _) } => :Greater
            | { &(:Native ref a), &(:Native ref b) } => std.cmp.default_compare(a, b)
            | { &(:Native _), _ } => :Less
            | { _, &(:Native _) } => :Greater
            | { &(:Fn ref a), &(:Fn ref b) } => std.cmp.default_compare(a, b)
            | { &(:Fn _), _ } => :Less
            | { _, &(:Fn _) } => :Greater
            | { &(:Array ref a), &(:Array ref b) } => (
                std.cmp.default_compare(&a^.element_ty, &b^.element_ty)
            )
            | { &(:Array _), _ } => :Less
            | { _, &(:Array _) } => :Greater
            | { &(:UnwindToken ref a), &(:UnwindToken ref b) } => (
                std.cmp.default_compare(&a^.result_ty, &b^.result_ty)
            )
            | { &(:UnwindToken _), _ } => :Less
            | { _, &(:UnwindToken _) } => :Greater
            | { &(:DelimitedContinuationToken ref a), &(:DelimitedContinuationToken ref b) } => (
                std.cmp.default_compare(&a^.result_ty, &b^.result_ty)
            )
            | { &(:DelimitedContinuationToken _), _ } => :Less
            | { _, &(:DelimitedContinuationToken _) } => :Greater
            | { &(:ContextObject), &(:ContextObject) } => :Equal
            | { &(:ContextObject), _ } => :Less
            | { _, &(:ContextObject) } => :Greater
        )
    ),
};

impl FnType as std.cmp.Ord = {
    .compare = (a, b) => with_return (
        match std.cmp.default_compare(&a^.is_closure, &b^.is_closure) with (
            | :Equal => ()
            | other => return other
        );
        match ({ a^.call_convention, b^.call_convention } :: { _, _ }) with (
            | { :None, :None } => ()
            | { :None, _ } => return :Less
            | { _, :None } => return :Greater
            | { :Some ref a, :Some ref b } => (
                match std.cmp.default_compare(a, b) with (
                    | :Equal => ()
                    | other => return other
                );
            )
        );
        let a_args_len = &a^.args |> ArrayList.length;
        let b_args_len = &b^.args |> ArrayList.length;
        match std.cmp.default_compare(&a_args_len, &b_args_len) with (
            | :Equal => ()
            | other => return other
        );
        match std.cmp.default_compare(&a^.result, &b^.result) with (
            | :Equal => ()
            | other => return other
        );
        for i in 0..a_args_len do (
            let arg_a = &a^.args |> ArrayList.at(i);
            let arg_b = &b^.args |> ArrayList.at(i);
            match std.cmp.default_compare(arg_a, arg_b) with (
                | :Equal => ()
                | other => return other
            );
        );
        :Equal
    ),
};

const NativeExprPart = newtype (
    | :Raw RcStr
    | :Interpolated Expr
);

const NativeExpr = newtype {
    .parts :: ArrayList.t[NativeExprPart],
};

const Literal = newtype (
    | :Bool Bool
    | :Int RcStr
    | :Float RcStr
    | :Char Char
    | :String RcStr
);

const CaptureMode = newtype (
    | :ByRef
    | :Move
);

const ExprShape = newtype (
    | :Unit
    | :Uninitialized
    | :Claim PlaceExpr
    | :Ref PlaceExpr
    | :Native NativeExpr
    | :Literal Literal
    | :Variant RcStr
    | :Stmt Expr
    | :Let {
        .name :: RcStr,
        .value :: Expr,
    }
    | :LetContextRef Expr
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
        .name :: RcStr,
        .value :: Expr,
    }
    | :Record ArrayList.t[Field]
    | :EnumIs {
        .enum :: Expr,
        .variant :: RcStr,
    }
    | :Loop Expr
    | :Unwindable {
        .token_ty :: Type,
        .token :: RcStr,
        .body :: Expr,
    }
    | :Unwind {
        .token :: Expr,
        .value :: Expr,
    }
    | :DelimitedContinuation {
        .capture_mode :: CaptureMode,
        .captures :: OrdMap.t[RcStr, Type],
        .token_ty :: Type,
        .resume_fn :: RcStr,
        .token :: RcStr,
        .body :: Expr,
    }
    | :CaptureContinuation {
        .token :: Expr,
        .continuation_ty_repr :: Type,
        .continuation :: RcStr,
        .resume_fn :: RcStr,
        .body :: Expr,
    }
    | :Defer Expr
    | :Array ArrayList.t[Expr]
    | :ConstructTypeInfo ConstructTypeInfo
);

const ConstructTypeInfo = newtype {
    .ty :: Type,
    .members :: ArrayList.t[ConstructTypeInfoMember],
};

const ConstructTypeInfoMember = newtype {
    .name :: RcStr,
    .ty :: Type,
    .type_info_const_name :: RcStr,
};

const Field = newtype {
    .name :: RcStr,
    .value :: Expr,
};

const Expr = newtype {
    .shape :: Rc.t[ExprShape],
    .ty :: Type,
    .span :: Span,
};

const PlaceExprShape = newtype (
    | :Ident RcStr
    | :Field {
        .obj :: PlaceExpr,
        .field :: RcStr,
    }
    | :Index {
        .list :: PlaceExpr,
        .index :: Expr,
    }
    | :CurrentContext RcStr
    | :Deref Expr
    | :Temp Expr
    | :ContextObject
);

const PlaceExpr = newtype {
    .shape :: Rc.t[PlaceExprShape],
    .ty :: Type,
    .span :: Span,
};

const FnArg = newtype {
    .name :: RcStr,
    .ty :: Type,
};

const FnDef = newtype {
    .capture_mode :: CaptureMode,
    .captures :: OrdMap.t[RcStr, Type],
    .call_convention :: Option.t[RcStr],
    .args :: ArrayList.t[FnArg],
    .result_ty :: Type,
    .body :: Expr,
    .span :: Span,
};

const Program = newtype {
    .types :: OrdMap.t[RcStr, TypeDef],
    .contexts :: OrdMap.t[RcStr, Type],
    .consts :: OrdMap.t[RcStr, Expr],
    .consts_order :: ArrayList.t[RcStr],
    .fns :: OrdMap.t[RcStr, FnDef],
};
