module:

const Iterable = [Item] newtype {
    .iter :: (Item -> ()) -> ()
};

const ReversibleIterable = [Item] newtype {
    .iter :: (Item -> ()) -> (),
    .rev :: () -> ReversibleIterable[Item],
};

const any = [T] (
    iter :: Iterable[T],
    predicate :: T -> Bool,
) -> Bool => with_return (
    for x in iter do (
        if predicate(x) then return true;
    );
    false
);

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
    iter.iter(x => (
        result = :Some (match result with (
            | :None => x
            | :Some prev => f(prev, x)
        ));
    ));
    result
);