module:

const Field = newtype {
    .name :: String,
    .ty :: TypeInfo,
};

const TypeInfo_Ref = newtype {
    .mutable :: Bool,
    .referenced :: Type,
};

const TypeInfo_Tuple = newtype {
    .unnamed :: std.collections.SList.t[Type],
    .named :: std.collections.SList.t[type { String, Type }],
};

const TypeInfo_Variant_Variant = newtype {
    .name :: String,
    .data :: Option.t[Type],
};

const TypeInfo_Variant = newtype {
    .variants :: std.collections.SList.t[TypeInfo_Variant_Variant],
};

const TypeInfo = newtype (
    | :Unit
    | :Bool
    | :Int32
    | :Int64
    | :Float64
    | :String
    | :Char
    | :Ref TypeInfo_Ref
    | :Variant TypeInfo_Variant
    | :Tuple TypeInfo_Tuple
    | :Ty
    | :Fn
    | :Generic
    | :Ast
    | :UnwindToken
    | :Target
    | :ContextTy
    | :CompilerScope
    | :Opaque
    | :Blocked
    | :Error
);
const type_info = (ty :: Type) -> TypeInfo => @cfg (
    | target.name == "interpreter" => (@native "reflection.type_info")(ty)
    | true => panic("reflection.type_info is comptime only")
);