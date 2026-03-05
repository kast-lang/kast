const Functor = [F :: [Type] -> Type] newtype {
    .fmap :: [A, B] (F[A], A -> B) -> F[B],
};

impl Option.t as Functor = {
    .fmap = Option.map,
};

impl ArrayList.t as Functor = {
    .fmap = [A, B] (a, f) => (
        let mut result = ArrayList.new();
        for &x in ArrayList.iter(&a) do (
            ArrayList.push_back(&mut result, f(x));
        );
        result
    ),
};

const increment_all = [F :: [Type] -> Type] (a :: F[Int32]) -> F[Int32] => (
    (F as Functor).fmap(a, el => el + 1)
);

let opt :: Option.t[Int32] = :Some 1;
let opt_incremented = increment_all(opt);
dbg.print({ .opt, .opt_incremented });

let mut list :: ArrayList.t[Int32] = ArrayList.new();
ArrayList.push_back(&mut list, 1);
ArrayList.push_back(&mut list, 2);
ArrayList.push_back(&mut list, 3);

let list_incremented = increment_all(list);
dbg.print(
    {
        .list = ArrayList.to_string(&list, &x => to_string(x)),
        .list_incremented = ArrayList.to_string(&list_incremented, &x => to_string(x))
    }
);
