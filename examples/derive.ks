use std.Ast;
use std.collections.SList;

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
        let @"impl" = match std.reflection.type_info(T) with (
            | :Bool => `(
                if $self^ then "true" else "false"
            )
            | :Int32 => `(
                let mut result = "";
                let mut x = $self^;
                if x < 0 then (
                    result += "-";
                    x = -x;
                );
                while x != 0 do (
                    let digit = x % 10;
                    let c = Char.from_code (Char.code('0') + digit);
                    result = std.String.to_string (c) + result;
                    x /= 10;
                );
                result
            )
            | :Int64 => `(0)
            | :Float64 => `(0)
            | :String => `("")
            | :Char => `(Char.from_code(0))
            | :Ref { .mutable, .referenced } => `(
                let mut result = "&";
                if mutable then (result += "mut ");
                result += (referenced as ToString).to_string($self^);
                result
            )
            | :Variant => panic("TODO")
            | :Tuple { .unnamed, .named } => (
                let add = `(add);
                let mut add_fields = `();
                for { i, ty } in unnamed |> SList.iter |> std.iter.enumerate do (
                    let i :: Ast = Ast.number_literal(i);
                    add_fields = `(
                        $add_fields;
                        $add(:None, &$self^.$i)
                    );
                );
                for { name, ty } in named |> SList.iter do (
                    let ident :: Ast = Ast.ident(name);
                    add_fields = `(
                        $add_fields;
                        $add(:Some name, &$self^.$ident)
                    );
                );
                `(
                    let mut result = "{";
                    let mut first = true;
                    let $add = [T](name, value :: &T) => (
                        if first then (
                            result += " ";
                            first = false;
                        ) else (
                            result += ", ";
                        );
                        if name is :Some name then (
                            result += ".";
                            result += name;
                            result += " = ";
                        );
                        result += to_str(value);
                    );
                    $add_fields;
                    result += " }";
                    result
                )
            )
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
@derive Int32 as ToString;

const Foo = newtype {
    &Bool,
    .int32 :: Int32,
    .bool :: Bool,
};

@derive Foo as ToString;
dbg.print(to_str(&true));
dbg.print(to_str(&&false));
dbg.print(to_str[Foo](&{ &false, .int32 = 123, .bool = true }));
