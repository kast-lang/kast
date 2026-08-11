module:

const Iterable = [Item] newtype {
    .iter :: (Item -> ()) -> ()
};

const ReversibleIterable = [Item] newtype {
    .iter :: (Item -> ()) -> (),
    .rev :: () -> ReversibleIterable[Item],
};

const find = [T] (
    iter :: std.iter.Iterable[T],
    predicate :: T -> Bool,
) -> Option.t[T] => with_return (
    for value in iter do (
        if predicate(value) then return :Some value;
    );
    :None
);

const any = [T] (
    iter :: Iterable[T],
    predicate :: T -> Bool,
) -> Bool => with_return (
    for x in iter do (
        if predicate(x) then return true;
    );
    false
);

const zip_TODO = [A, B] (
    a :: Iterable[A],
    b :: Iterable[B],
) -> Iterable[type { A, B }] => {
    .iter = consume => (
        let element_of_a = _;
        let element_of_b = _;
        consume({ element_of_a, element_of_b });
    )
};

const all = [T] (
    iter :: Iterable[T],
    predicate :: T -> Bool,
) -> Bool => with_return (
    for x in iter do (
        if not predicate(x) then return false;
    );
    true
);

const map = [A, B] (
    iter :: Iterable[A],
    f :: A -> B,
) -> Iterable[B] => {
    .iter = consume => (
        iter.iter(a => consume(f(a)))
    )
};

const enumerate = [T] (
    iter :: Iterable[T]
) -> Iterable[type { Int32, T }] => (
    let mut i = 0;
    {
        .iter = consume => (
            iter.iter(
                x => (
                    consume({ i, x });
                    i += 1;
                )
            );
        ),
    }
);

const reduce = [T] (
    iter :: Iterable[T],
    f :: (T, T) -> T,
) -> Option.t[T] => (
    let mut result = :None;
    iter.iter(
        x => (
            result = :Some (
                match result with (
                    | :None => x
                    | :Some prev => f(prev, x)
                )
            );
        )
    );
    result
);
