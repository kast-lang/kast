module:
const list = [T] type (
    | :Empty
    | :Cons (
        .head :: T,
        .tail :: list[T],
    )
);
const t = list;
const create = [T] () -> list[T] => :Empty;
const add = [T] (a :: list[T], value :: T) -> list[T] => (
    match a with (
        | :Empty => :Cons (
            .head = value,
            .tail = :Empty,
        )
        | :Cons (.head, .tail) => :Cons (
            .head,
            .tail = add[T] (tail, value),
        )
    )
);
const iter = [T] (a :: list[T], f :: (T -> ())) -> () => (
    let @"impl" = (
        module:
        let print_inner = a => match a with (
            | :Empty => ()
            | :Cons (.head, .tail) => (
                f head;
                print_inner tail;
            )
        );
    );
    @"impl".print_inner a
);
