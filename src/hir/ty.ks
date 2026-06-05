const TyShape = newtype (
    | :Unit
    | :Int
    | :Type
    | :Fn {
        .args :: ArrayList.t[Ty],
        .result :: Ty,
    }
);

impl TyShape as module = (
    module:

    const print = (self :: &TyShape) => (
        let output = @current Output;
        match self^ with (
            | :Unit => output.write("()")
            | :Int => output.write("Int")
            | :Type => output.write("Type")
            | :Fn { .args = ref args, .result = ref result } => (
                if args |> ArrayList.length != 1 then (
                    output.write("(");
                );
                for { i, arg } in args |> ArrayList.iter |> std.iter.enumerate do (
                    if i != 0 then (
                        output.write(", ");
                    );
                    Ty.print(arg);
                );
                if args |> ArrayList.length != 1 then (
                    output.write(")");
                );
                output.write(" -> ");
                Ty.print(result);
            )
        );
    );
);

const Ty = newtype {
    .shape :: TyShape,
};

impl Ty as module = (
    module:

    const UNIT :: Ty = { .shape = :Unit };
    const INT :: Ty = { .shape = :Int };
    const TYPE :: Ty = { .shape = :Type };

    const print = (self :: &Ty) => (
        TyShape.print(&self^.shape);
    );
);
