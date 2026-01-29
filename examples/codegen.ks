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

const generate = (what :: WhatToGenerate) -> Ast => (
    let name :: Ast = @parse what.name;
    let mut args = :None;
    for &arg in List.iter(&what.args) do (
        args = match args with (
            | :None => :Some `(arg)
            | :Some args => :Some `($args, arg)
        );
    );
    let args = match args with (
        | :Some ast => ast
        | :None => `()
    );
    `(
        @eval dbg.print(type { $args });
        const $name = (...args :: { $args }) -> what.returns => (
            (@native "(ctx,...args)=>" + what.name + "(...args)")(...args)
        )
    )
);

# include_ast generate(
#     {
#         .name = "parseFloat",
#         .args = (
#             let mut args = List.create();
#             &mut args |> List.push_back(String);
#             args
#         ),
#         .returns = Float64,
#     }
# );
# dbg.print(parseFloat("123"));
