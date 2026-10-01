
impl syntax (@interpolate_String(ast)) = (
    let mut result = `();
    for part in (
        std.Ast.get_comma_separated_list(ast)
            |> std.collections.ArrayList.into_iter
    ) do (
        result = `(
            $result;
            StringBuilder.add_String(String.to_string($part));
        );
    );
    `(StringBuilder.build(() => $result))
);
