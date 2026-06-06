const TyShape = newtype (
    | :Unit
    | :Int
    | :String
    | :Type
    | :Fn {
        .args :: ArrayList.t[Ty],
        .result :: Ty,
    }
    | :Ast
);

impl TyShape as module = (
    module:

    const print = (self :: &TyShape) => (
        let output = @current Output;
        match self^ with (
            | :Unit => output.write("()")
            | :Int => output.write("Int")
            | :String => output.write("String")
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
            | :Ast => output.write("Ast")
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
    const STRING :: Ty = { .shape = :String };
    const TYPE :: Ty = { .shape = :Type };
    const AST :: Ty = { .shape = :Ast };

    const print = (self :: &Ty) => (
        TyShape.print(&self^.shape);
    );
);
