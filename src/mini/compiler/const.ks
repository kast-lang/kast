use (import "./common.ks").*;

module:

const process_const_declaration = (
    name :: String,
    ast :: Ast.t,
) -> ConstDeclaration => with_return (
    if ast.shape is :Rule { .rule, .root } then (
        if rule.name == "native const" then (
            let { name, ty } = root
                |> AstHelpers.expect_two_children(:Some { "name", "type" });
            return {
                .ty = (@current Compiler).parse_type(ty),
                .value = :None,
            };
        );
    );
    let { name_and_type, value } = ast
        |> AstHelpers.expect_rule("const")
        |> AstHelpers.expect_two_children(:Some { "name", "value" });
    let { name, ty } = name_and_type
        |> AstHelpers.expect_rule("type ascribe")
        |> AstHelpers.expect_two_children(:Some { "expr", "type" });
    {
        .ty = (@current Compiler).parse_type(ty),
        .value = :Some value,
    }
);

const process_const = (
    name :: String,
    decl :: ConstDeclaration,
) -> Ir.Expr => (
    if decl.value is :Some value then (
        (@current Compiler).parse_expr(:Some decl.ty, value)
    ) else (
        panic("Const decl has no value (native const), we aren't supposed to process it")
    )
);
