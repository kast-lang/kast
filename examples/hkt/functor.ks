use std.prelude.*;

const Functor = [F :: [type] -> type] type (
    .fmap :: [A, B] (F[A], A -> B) -> F[B],
);

impl Option.t as Functor = (
    .fmap = Option.map,
);

impl List.t as Functor = (
    .fmap = [A, B] (a, f) => (
        let mut result = List.create();
        for &x in List.iter(&a) do (
            List.push_back(&mut result, f(x));
        );
        result
    ),
);

const increment_all = [F :: [type] -> type] (a :: F[Int32]) -> F[Int32] => (
    (F as Functor).fmap(a, el => el + 1)
);

let opt :: Option.t[Int32] = :Some(1);
let opt_incremented = increment_all(opt);
dbg.print(.opt, .opt_incremented);

let mut list :: List.t[Int32] = List.create();
List.push_back(&mut list, 1);
List.push_back(&mut list, 2);
List.push_back(&mut list, 3);

let list_incremented = increment_all(list);
dbg.print(
    .list = List.to_string(&list, &x => to_string(x)),
    .list_incremented = List.to_string(&list_incremented, &x => to_string(x))
);
