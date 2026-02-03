const reflection = (
    module:
    
    const Field = newtype {
        .name :: String,
        .ty :: TypeInfo,
    };
    
    const TypeInfo_Ref = newtype {
        .mutable :: Bool,
        .referenced :: Type,
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
        | :Variant
        | :Tuple
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
    const type_info :: Type -> TypeInfo = @native "reflection.type_info";
);

dbg.print(reflection.type_info(Int32));

@syntax "@field" 10 @wrap never = "@field" "(" obj "," " " field ")";
impl syntax (@field(obj, field)) = `(
    # TODO
    _
);

use std.Ast;

const Derivable = [Trait] type (Type -> Ast);

@syntax "@derive" 10 @wrap never = "@derive" " " ty " " "as" " " trait;
impl syntax (@derive ty as trait) = `(
    include_ast (($trait as Derivable)($ty))
);

const ToString = [Self] newtype {
    .to_string :: &Self -> String,
};
const to_str = [T] (x :: &T) -> String => (
    (T as ToString).to_string(x)
);

@eval (
    impl ToString as Derivable = (T :: Type) -> Ast => (
        let self = `(self);
        let @"impl" = match reflection.type_info(T) with (
            | :Bool => `(
                if $self^ then "true" else "false"
            )
            | :Int32 => `(0)
            | :Int64 => `(0)
            | :Float64 => `(0)
            | :String => `("")
            | :Char => `(Char.from_code(0))
            | :Ref { .mutable, .referenced } => `(
                let mut result = "&";
                if mutable then ( result += "mut ");
                result += (referenced as ToString).to_string($self^);
                result
            )
            | :Variant => panic("TODO")
            | :Tuple => panic("TODO")
            | :Ty => panic("TODO")
            | :Fn => panic("TODO")
            | :Generic => panic("TODO")
            | :Ast => panic("TODO")
            | :UnwindToken => panic("TODO")
            | :Target => panic("TODO")
            | :ContextTy => panic("TODO")
            | :CompilerScope => panic("TODO")
            | :Opaque => panic("TODO")
            | :Blocked => panic("TODO")
            | :Error => panic("TODO")
        );
        `(
            impl T as ToString = {
                .to_string = ($self) => $(@"impl"),
            }
        )
    );
);

@derive Bool as ToString;
# rust: impl<T> ToString for &T
# kast: impl[T] type(&T) as ToString
# @derive [T] type (&T) as ToString;
@derive type (&Bool) as ToString;

dbg.print(to_str(&true));
dbg.print(to_str(&&false));
