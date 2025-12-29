const Int32 = @native "int32";
const Int64 = @native "int64";

const Monad = [M :: [_ :: type] type] newtype (
    .flat_map :: [A] (M[A], (A -> M[Int32])) -> M[Int32],
);

const Option = [T :: type] newtype (
    | :None
    | :Some T
);

let opt :: Option[Int32] = :Some 123;
let x = (_ as Monad).flat_map[Int64];
#  (opt, x => :Some x);
