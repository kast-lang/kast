use std.Ast;

const Foo = newtype {
    .a :: Int32,
    .b :: String,
    .c :: Float32,
};

const static_for_impl = (
    T :: Type,
    field :: Ast,
    value :: Ast,
    body :: Ast,
) -> Ast => (
    match T |> std.reflection.type_info with (
        | :Tuple { .unnamed, .named } => (
            let mut result = `();
            for { field_name, field_ty } in named
                |> std.collections.SList.into_iter
            do (
                let field_ident = std.Ast.ident(field_name);
                result = `(
                    $result;
                    let $field = { field_name, $value.$field_ident };
                    $body;
                );
            );
            result
        )
    )
);

@syntax "static_for" 10 @wrap never = "@static_for" " " field " " "in" " " value " " "do" " " body;
impl syntax (@static_for field in value do body) = `(
    let value = $value;
    include_ast static_for_impl(
        typeof value,
        field,
        `(value),
        body,
    );
);

let foo :: Foo = {
    .a = 1,
    .b = String.from_str("hi"),
    .c = 3,
};
@static_for { field_name, field_value } in foo do (
    dbg.print({ field_name, typeof(field_value), field_value });
);
