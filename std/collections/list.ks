module:
const list = [T] type (
    | :Empty
    | :Cons (
        .head :: T,
        .tail :: list[T],
    )
);
const t = list;
const create = [createT] () -> list[createT] => :Empty;
const add = [addT] (a :: list[addT], value :: addT) -> list[addT] => (
    match a with (
        | :Empty => :Cons (
            .head = value,
            .tail = :Empty,
        )
        | :Cons (.head, .tail) => :Cons (
            .head,
            .tail = add[addT] (tail, value),
        )
    )
);
const print = [printT] (print_value :: (printT -> ()), a :: list[printT]) -> () => (
    let @"impl" = (
        module:
        let print_inner = a => match a with (
            | :Empty => ()
            | :Cons (.head, .tail) => (
                print_value head;
                print_inner tail;
            )
        );
    );
    @"impl".print_inner a
);
