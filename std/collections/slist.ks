module:

const t = [T] newtype (
    | :Nil
    | :Cons {
        .value :: T,
        .tail :: t[T],
    }
);

const iter = [T] (list :: t[T]) -> std.iter.Iterable[T] => {
    .iter = f => match list with (
        | :Nil => ()
        | :Cons { .value, .tail } => (
            f(value);
            iter(tail).iter(f);
        )
    ),
};