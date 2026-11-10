const Int = Int32;

const ValueShape = newtype (
    | :Unit
    | :Bool Bool
    | :Int Int
    | :String RcStr
    | :Type Ty
    | :Fn Fn
    | :NativeFn NativeFn
    | :Ast Ast.t
);

const Fn = newtype {
    .args :: ArrayList.t[Pattern],
    .body :: Expr,
};

const NativeFn = newtype {
    .name :: RcStr,
    .@"impl" :: (ArrayList.t[ValueWithSpan], .caller :: Span) -> Value,
};

impl ValueShape as module = (
    module:

    const print = (self :: &ValueShape) => (
        match self^ with (
            | :Unit => locked_output.write_str("()")
            | :Bool b => locked_output.write_str(&to_string(b) |> as_str)
            | :Int x => locked_output.write_str(&to_string(x) |> as_str)
            | :String ref s => locked_output.write_escaped(s)
            | :Type ref ty => (
                locked_output.write_str("type ");
                Ty.print(ty)
            )
            | :Fn _ => (
                locked_output.write_str("<fn>");
            )
            | :NativeFn ref f => (
                locked_output.write_str("<native ");
                locked_output.write(&f^.name);
                locked_output.write_str(">")
            )
            | :Ast ref ast => (
                Ast.print(ast);
            )
        );
    );
);

const Value = Rc.t[newtype {
    .shape :: ValueShape,
    .ty :: Ty,
}];

impl Value as module = (
    module:

    const UNIT :: Value = Rc.new({ .shape = :Unit, .ty = Ty.UNIT });
    const TRUE :: Value = Rc.new({ .shape = :Bool true, .ty = Ty.BOOL });
    const FALSE :: Value = Rc.new({ .shape = :Bool false, .ty = Ty.BOOL });

    const print = (value :: &Value) => (
        ValueShape.print(&value^^.shape);
        locked_output.write_str(" :: ");
        Ty.print(&value^^.ty);
    );
);

const ValueWithSpan = newtype {
    Value,
    .span :: Span,
};
