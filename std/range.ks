module:

const Number = [Self] newtype {
    .ZERO :: Self,
    .ONE :: Self,
};

impl Int32 as Number = {
    .ZERO = 0,
    .ONE = 1,
};

impl UInt32 as Number = {
    .ZERO = 0,
    .ONE = 1,
};

const range = [T] (
    start :: T,
    end :: T,
) -> std.iter.ReversibleIterable[T] => (
    let ONE = (T as Number).ONE;
    let forward = consumer => (
        let mut i = start;
        while i < end do (
            consumer(i);
            i += ONE;
        );
    );
    let backward = consumer => (
        let mut i = end;
        while i > start do (
            i -= ONE;
            consumer(i);
        );
    );
    (
        module:
        let construct = (.forward, .backward) => {
            .iter = forward,
            .rev = () => construct(.forward = backward, .backward = forward),
        };
    ).construct(.forward, .backward)
)
