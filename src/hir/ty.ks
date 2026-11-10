const TyShape = newtype (
    | :Unit
    | :Bool
    | :Int
    | :String
    | :Type
    | :Fn FnTy
    | :Ast
);

const FnTy = newtype {
    .args :: ArrayList.t[Ty],
    .result :: Ty,
};

impl TyShape as module = (
    module:

    const print = (self :: &TyShape) => (
        match self^ with (
            | :Unit => locked_output.write_str("()")
            | :Bool => locked_output.write_str("Bool")
            | :Int => locked_output.write_str("Int")
            | :String => locked_output.write_str("String")
            | :Type => locked_output.write_str("Type")
            | :Fn { .args = ref args, .result = ref result } => (
                if args |> ArrayList.length != 1 then (
                    locked_output.write_str("(");
                );
                for { i, arg } in args |> ArrayList.iter |> std.iter.enumerate do (
                    if i != 0 then (
                        locked_output.write_str(", ");
                    );
                    Ty.print(arg);
                );
                if args |> ArrayList.length != 1 then (
                    locked_output.write_str(")");
                );
                locked_output.write_str(" -> ");
                Ty.print(result);
            )
            | :Ast => locked_output.write_str("Ast")
        );
    );
);

const Ty = Rc.t[newtype {
    .shape :: TyShape,
}];

impl Ty as module = (
    module:

    const UNIT :: Ty = Rc.new({ .shape = :Unit });
    const BOOL :: Ty = Rc.new({ .shape = :Bool });
    const INT :: Ty = Rc.new({ .shape = :Int });
    const STRING :: Ty = Rc.new({ .shape = :String });
    const TYPE :: Ty = Rc.new({ .shape = :Type });
    const AST :: Ty = Rc.new({ .shape = :Ast });

    const print = (self :: &Ty) => (
        TyShape.print(&self^^.shape);
    );
);
