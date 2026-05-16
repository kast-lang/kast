use (import "../../span.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;

module:

const TypeDefShape = newtype (
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

const TypeDef = newtype {
    .shape :: TypeDefShape,
    .native :: Bool,
};

const FnType = newtype {
    .is_closure :: Bool,
    .call_convention :: Option.t[String],
    .args :: ArrayList.t[Type],
    .result :: Type,
};

const Type = newtype (
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
    | :Named String
    | :Alias {
        .name :: String,
        .resolved :: Type,
    }
    | :Fn FnType
    | :Native String
    | :List {
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

const resolve_type_alias = (ty :: &Type) -> &Type => (
    match ty^ with (
        | :Alias { .name = _, .resolved = ref resolved } => resolved
        | _ => ty
    )
);

const type_repr = (ty :: &Type) -> &Type => (
    match resolve_type_alias(ty)^ with (
        | :UnwindToken { .repr = ref repr, ... } => repr
        | :List { .repr = ref repr, ... } => repr
        | _ => ty
    )
);

# TODO derive
const compare_type = (
    a :: &Type,
    b :: &Type,
) -> std.cmp.Ordering => with_return (
    let a = resolve_type_alias(a);
    let b = resolve_type_alias(b);
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
        | { :Int, :Int } => :Equal
        | { :Int, _ } => :Less
        | { _, :Int } => :Greater
        | { :UInt, :UInt } => :Equal
        | { :UInt, _ } => :Less
        | { _, :UInt } => :Greater
        | { :IntSpecific a, :IntSpecific b } => (
            if a.signed != b.signed then (
                return std.cmp.default_compare[Bool](a.signed, b.signed);
            );
            std.cmp.default_compare[Int32](a.bits, b.bits)
        )
        | { :IntSpecific _, _ } => :Less
        | { _, :IntSpecific _ } => :Greater
        | { :Float32, :Float32 } => :Equal
        | { :Float32, _ } => :Less
        | { _, :Float32 } => :Greater
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
        | { :Fn ref a, :Fn ref b } => compare_fn_type(a, b)
        | { :Fn _, _ } => :Less
        | { _, :Fn _ } => :Greater
        | { :List ref a, :List ref b } => (
            compare_type(&a^.element_ty, &b^.element_ty)
        )
        | { :List _, _ } => :Less
        | { _, :List _ } => :Greater
        | { :UnwindToken ref a, :UnwindToken ref b } => (
            compare_type(&a^.result_ty, &b^.result_ty)
        )
        | { :UnwindToken _, _ } => :Less
        | { _, :UnwindToken _ } => :Greater
        | { :DelimitedContinuationToken ref a, :DelimitedContinuationToken ref b } => (
            compare_type(&a^.result_ty, &b^.result_ty)
        )
        | { :DelimitedContinuationToken _, _ } => :Less
        | { _, :DelimitedContinuationToken _ } => :Greater
        | { :ContextObject, :ContextObject } => :Equal
        | { :ContextObject, _ } => :Less
        | { _, :ContextObject } => :Greater
    )
);

const compare_fn_type = (
    a :: &FnType,
    b :: &FnType,
) -> std.cmp.Ordering => with_return (
    let cmp = std.cmp.default_compare[Bool](a^.is_closure, b^.is_closure);
    if cmp is :Equal then () else (
        return cmp;
    );
    match { a^.call_convention, b^.call_convention } with (
        | { :None, :None } => ()
        | { :None, _ } => return :Less
        | { _, :None } => return :Greater
        | { :Some a, :Some b } => (
            let cmp = std.cmp.default_compare[String](a, b);
            if cmp is :Equal then () else (
                return cmp;
            );
        )
    );
    let a_args_len = &a^.args |> ArrayList.length;
    let b_args_len = &b^.args |> ArrayList.length;
    let cmp = std.cmp.default_compare(a_args_len, b_args_len);
    if cmp is :Equal then () else (
        return cmp;
    );
    let cmp = compare_type(&a^.result, &b^.result);
    if cmp is :Equal then () else (
        return cmp;
    );
    for i in 0..a_args_len do (
        let arg_a = &a^.args |> ArrayList.at(i);
        let arg_b = &b^.args |> ArrayList.at(i);
        let cmp = compare_type(arg_a, arg_b);
        if cmp is :Equal then () else (
            return cmp;
        );
    );
    :Equal
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
    | :Int String
    | :Float String
    | :Char Char
    | :String String
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
    | :Variant String
    | :Stmt Expr
    | :Let {
        .name :: String,
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
        .name :: String,
        .value :: Expr,
    }
    | :Record ArrayList.t[Field]
    | :EnumIs {
        .enum :: Expr,
        .variant :: String,
    }
    | :Loop Expr
    | :Unwindable {
        .token_ty_repr :: Type,
        .token :: String,
        .body :: Expr,
    }
    | :Unwind {
        .token :: Expr,
        .value :: Expr,
    }
    | :DelimitedContinuation {
        .capture_mode :: CaptureMode,
        .captures :: OrdMap.t[String, Type],
        .token_ty_repr :: Type,
        .resume_fn :: String,
        .token :: String,
        .body :: Expr,
    }
    | :CaptureContinuation {
        .token :: Expr,
        .continuation_ty_repr :: Type,
        .continuation :: String,
        .resume_fn :: String,
        .body :: Expr,
    }
    | :Defer Expr
    | :List ArrayList.t[Expr]
    | :ConstructTypeInfo ConstructTypeInfo
);

const ConstructTypeInfo = newtype {
    .ty :: Type,
    .members :: ArrayList.t[ConstructTypeInfoMember],
};

const ConstructTypeInfoMember = newtype {
    .name :: String,
    .ty :: Type,
    .type_info_const_name :: String,
};

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
    | :ContextObject
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
    .capture_mode :: CaptureMode,
    .captures :: OrdMap.t[String, Type],
    .call_convention :: Option.t[String],
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
