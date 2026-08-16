use (import "./json/_lib.ks").*;
use (import "./token.ks").*;
use std.Ast;
use std.collections.OrdMap;
use std.collections.SList;

const to_float64 = [T] (x :: T) -> Float64 => (
    parse(to_string(x))
);

module:

const Serialize = (
    module:

    const ToJson = [Self] newtype {
        .to_json :: Self -> Json.t,
    };

    const derive_to_json = (T :: Type) -> Ast => @cfg (
        | target.name == "interpreter" => (
            let self = `(self);
            let to_json_impl = match std.reflection.type_info(T) with (
                | :Bool => `(
                    :Bool $self
                )
                | :Int32 => `(
                    :Number to_float64($self)
                )
                | :Int64 => `(
                    :Number to_float64($self)
                )
                | :Float64 => `(
                    :Number $self
                )
                | :String => `(
                    :String $self
                )
                | :Char => `(
                    :String to_string($self)
                )
                | :Ref { .mutable, .referenced } => panic("TODO to_json Ref")
                | :Variant { .variants } => (
                    let fields = `(fields);
                    let mut match_variants = `();
                    for { .name, .data } in variants |> SList.into_iter do (
                        let variant_ident = std.Ast.ident(name);
                        let match_variant = match data with (
                            | :Some data_ty => `(
                                :$variant_ident data => (
                                    OrdMap.add(
                                        &mut $fields,
                                        name,
                                        (data_ty as ToJson).to_json(data),
                                    );
                                )
                            )
                            | :None => `(
                                :$variant_ident => (
                                    OrdMap.add(&mut $fields, name, :Null);
                                )
                            )
                        );
                        match_variants = `($match_variants | $match_variant);
                    );
                    `(
                        let mut $fields = OrdMap.new();
                        match $self with (
                            $match_variants
                        );
                        :Object $fields
                    )
                )
                | :Tuple { .unnamed, .named } => (
                    let fields = `(fields);
                    let mut init_fields = `();
                    for { i, field_ty } in unnamed |> SList.into_iter |> std.iter.enumerate do (
                        let field_name = to_string(i);
                        let field_ident = std.Ast.number_literal(i);
                        init_fields = `(
                            $init_fields;
                            OrdMap.add(
                                &mut $fields,
                                field_name,
                                (field_ty as ToJson).to_json($self.$field_ident),
                            );
                        );
                    );
                    for { field_name, field_ty } in named |> SList.into_iter do (
                        let field_ident = std.Ast.ident(field_name);
                        init_fields = `(
                            $init_fields;
                            OrdMap.add(
                                &mut $fields,
                                field_name,
                                (field_ty as ToJson).to_json($self.$field_ident),
                            );
                        );
                    );
                    `(
                        let mut $fields = OrdMap.new();
                        $init_fields;
                        :Object $fields
                    )
                )
                | :Ty => panic("TODO to_json Ty")
                | :Fn => panic("TODO to_json Fn")
                | :Generic => panic("TODO to_json Generic")
                | :Ast => panic("TODO to_json Ast")
                | :UnwindToken => panic("TODO to_json UnwindToken")
                | :Target => panic("TODO to_json Target")
                | :ContextTy => panic("TODO to_json ContextTy")
                | :CompilerScope => panic("TODO to_json CompilerScope")
                | :Opaque => panic("TODO to_json Opaque")
                | :Blocked => panic("TODO to_json Blocked")
                | :Error => panic("TODO to_json Error")
            );
            `(
                impl T as ToJson = {
                    .to_json = $self => $to_json_impl,
                }
            )
        )
        | true => panic("comptime only")
    );
# const do_impl = () => (
#     include_ast derive_to_json(Token.t);
#     include_ast derive_to_json(Token.Shape.t);
# );
# const as_json = [T] (value :: T) -> Json.t => (
# (T as ToJson).to_json(value)
# );
);
