module:

const t = [T] newtype (
    | :Nil
    | :Cons {
        .value :: T,
        .tail :: t[T],
    }
);

const into_iter = [T] (list :: t[T]) -> std.iter.Iterable[T] => {
    .iter = f => match list with (
        | :Nil => ()
        | :Cons { .value, .tail } => (
            f(value);
            into_iter(tail).iter(f);
        )
    ),
};

const iter = [T] (list :: &t[T]) -> std.iter.Iterable[type (&T)] => {
    .iter = f => match list^ with (
        | :Nil => ()
        | :Cons { .value = ref value, .tail = ref tail } => (
            f(value);
            iter(tail).iter(f);
        )
    ),
};