const Int = Int32;

const ValueShape = newtype (
    | :Unit
    | :Bool Bool
    | :Int Int
    | :String String
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
    .name :: String,
    .@"impl" :: (ArrayList.t[ValueWithSpan], .caller :: Span) -> Value,
};

impl ValueShape as module = (
    module:

    const print = (self :: &ValueShape) => (
        let output = @current Output;
        match self^ with (
            | :Unit => output.write("()")
            | :Bool b => output.write(to_string(b))
            | :Int x => output.write(to_string(x))
            | :String s => output.write(String.escape(s))
            | :Type ref ty => (
                output.write("type ");
                Ty.print(ty)
            )
            | :Fn _ => (
                output.write("<fn>");
            )
            | :NativeFn ref f => (
                output.write("<native ");
                output.write(f^.name);
                output.write(">")
            )
            | :Ast ref ast => (
                Ast.print(ast);
            )
        );
    );
);

const Value = newtype {
    .shape :: ValueShape,
    .ty :: Ty,
};

impl Value as module = (
    module:

    const UNIT :: Value = { .shape = :Unit, .ty = Ty.UNIT };

    const print = (value :: &Value) => (
        let output = @current Output;
        ValueShape.print(&value^.shape);
        output.write(" :: ");
        Ty.print(&value^.ty);
    );
);

const ValueWithSpan = newtype {
    Value,
    .span :: Span,
};
