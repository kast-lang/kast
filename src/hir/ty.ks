const TyShape = newtype (
    | :Unit
    | :Int
    | :Type
);

impl TyShape as module = (
    module:

    const print = (self :: &TyShape) => (
        let output = @current Output;
        match self^ with (
            | :Unit => output.write("()")
            | :Int => output.write("Int")
            | :Type => output.write("Type")
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
