const Ast = @native "Ast";

impl Ast as module = (
    module:

    const StringPart = newtype (
        | :Content {
            .content :: String,
        }
        | :Interpolate {
            .value :: Ast,
        }
    );

    const Shape = newtype (
        | :String {
            .parts :: std.collections.ArrayList.t[StringPart],
        }
        | :Unknown
    );

    const shape = (ast :: Ast) -> Ast.Shape => @cfg (
        | target.name == "interpreter" => (@native "Ast.shape")(ast)
        | true => panic("comptime only")
    );

    const number_literal = (x :: Int32) -> Ast => @cfg (
        | target.name == "interpreter" => (@native "syntax.number_literal")(x)
        | true => panic("comptime only")
    );

    const ident = (name :: String) -> Ast => @cfg (
        | target.name == "interpreter" => (@native "syntax.ident")(name)
        | true => panic("comptime only")
    );

    const get_comma_separated_list = (ast :: Ast) -> std.collections.ArrayList.t[Ast] => @cfg (
        | target.name == "interpreter" => (@native "Ast.get_comma_separated_list")(ast)
        | true => panic("comptime only")
    );
);
