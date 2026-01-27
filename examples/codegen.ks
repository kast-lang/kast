# const parseFloat = (s :: String) -> Float64 => (@native "(ctx,s)=>parseFloat(s)")(s);
const WhatToGenerate = newtype {
    .name :: String,
    .args :: List.t[Type],
    .returns :: Type,
};

@syntax "@parse" 10 @wrap never = "@parse" " " code;
impl syntax (@parse code) = `(
    (@native "@parse")($code)
);

use std.Ast;

const type_to_ast = (t :: Type) -> Ast => (
    let x = 2 + 2;
    # TODO
    `(
        x
    )
);

const type_to_ast_string = (t :: Type) -> String => (
    if t == String then (
        "String"
    ) else if t == Float64 then (
        "Float64"
    ) else (
        panic("i dont know them")
    )
);

const generate = (what :: WhatToGenerate) -> Ast => (
    let mut code = "const " + what.name + " = (";
    code += "...args :: {";
    for &arg in List.iter(&what.args) do (
        code += type_to_ast_string(arg);
        code += ", ";
    );
    code += "}) -> " + type_to_ast_string(what.returns);
    code += " => (\n";
    code += "(@native \"(ctx,...args)=>" + what.name + "(...args)\")(...args)";
    code += "\n)";
    @parse code
);

include_ast generate(
    {
        .name = "parseFloat",
        .args = (
            let mut args = List.create();
            &mut args |> List.push_back(String);
            args
        ),
        .returns = Float64,
    }
);
dbg.print(parseFloat("123"));
