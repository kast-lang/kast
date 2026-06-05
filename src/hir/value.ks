const Int = Int32;

const ValueShape = newtype (
    | :Unit
    | :Int Int
    | :Type Ty
    | :NativeFn NativeFn
);

const NativeFn = newtype {
    .name :: String,
    .@"impl" :: (ArrayList.t[Value], .caller :: Span) -> Value,
};

impl ValueShape as module = (
    module:

    const print = (self :: &ValueShape) => (
        let output = @current Output;
        match self^ with (
            | :Unit => output.write("()")
            | :Int x => output.write(to_string(x))
            | :Type ref ty => (
                output.write("type ");
                Ty.print(ty)
            )
            | :NativeFn ref f => (
                output.write("<native ");
                output.write(f^.name);
                output.write(">")
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
