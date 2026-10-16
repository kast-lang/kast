module:

const AddOutput = [{ LHS, RHS }] newtype {
    .Output :: Type,
};

const Add = [{ LHS, RHS }] newtype {
    .add :: (LHS, RHS) -> ({ LHS, RHS } as AddOutput).Output,
};

impl { Int32, Int32 } as AddOutput = {
    .Output = Int32,
};
impl { Int32, Int32 } as Add = {
    .add = (a, b) => a + b,
};
