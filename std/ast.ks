const Ast = @native "Ast";

impl Ast as module = (
    module:

    const number_literal = (x :: Int32) -> Ast => @cfg (
        | target.name == "interpreter" => (@native "syntax.number_literal")(x)
        | true => panic("comptime only")
    );

    const ident = (name :: String) -> Ast => @cfg (
        | target.name == "interpreter" => (@native "syntax.ident")(name)
        | true => panic("comptime only")
    );
);