const TyShape = newtype (
    | :Unit
    | :Bool
    | :Int
    | :String
    | :Type
    | :Fn FnTy
    | :Tuple TupleTy
    | :Ast
);

const TupleTy = Rc.t[newtype {
    .fields :: Tuple.t[Ty],
}];

impl TupleTy as module = (
    module:

    const print = (self :: &TupleTy) => (
        Tuple.print(
            &self^^.fields,
            .open = "{",
            .close = "}",
            .delimiter = ",",
            .before_field_name = ".",
            .after_field_name = " :: ",
            .print_value = Ty.print,
        );
    );
);

const FnTy = Rc.t[newtype {
    .args :: ArrayList.t[Ty],
    .result :: Ty,
}];

impl FnTy as module = (
    module:

    const print = (self :: &FnTy) => (
        if &self^^.args |> ArrayList.length != 1 then (
            locked_output.write_str("(");
        );
        for { i, arg } in &self^^.args |> ArrayList.iter |> std.iter.enumerate do (
            if i != 0 then (
                locked_output.write_str(", ");
            );
            Ty.print(arg);
        );
        if &self^^.args |> ArrayList.length != 1 then (
            locked_output.write_str(")");
        );
        locked_output.write_str(" -> ");
        Ty.print(&self^^.result);
    );
);

impl TyShape as module = (
    module:

    const print = (self :: &TyShape) => (
        match self^ with (
            | :Unit => locked_output.write_str("()")
            | :Bool => locked_output.write_str("Bool")
            | :Int => locked_output.write_str("Int")
            | :String => locked_output.write_str("String")
            | :Type => locked_output.write_str("Type")
            | :Fn ref f => FnTy.print(f)
            | :Tuple ref t => TupleTy.print(t)
            | :Ast => locked_output.write_str("Ast")
        );
    );
);

const Ty = Inference.Var.t[TyShape];

impl Ty as module = (
    module:

    const UNIT :: Ty = Inference.Var.new_inferred(:Unit);
    const BOOL :: Ty = Inference.Var.new_inferred(:Bool);
    const INT :: Ty = Inference.Var.new_inferred(:Int);
    const STRING :: Ty = Inference.Var.new_inferred(:String);
    const TYPE :: Ty = Inference.Var.new_inferred(:Type);
    const AST :: Ty = Inference.Var.new_inferred(:Ast);

    const print = (self :: &Ty) => (
        Inference.Var.print(self, TyShape.print);
    );

    const new_inferred = Inference.Var.new_inferred[TyShape];
    const new_not_inferred = Inference.Var.new_not_inferred[TyShape];
    const ensure_inferred_or = Inference.Var.ensure_inferred_or[TyShape];
);
