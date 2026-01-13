module:

const range = (start :: Int32, end :: Int32) -> std.iter.ReversibleIterable[Int32] => (
    let forward = consumer => (
        let mut i = start;
        while i < end do (
            consumer(i);
            i += 1;
        );
    );
    let backward = consumer => (
        let mut i = end;
        while i > start do (
            i -= 1;
            consumer(i);
        );
    );
    (
        module:
        let construct = (.forward, .backward) => (
            .iter = forward,
            .rev = () => construct(.forward = backward, .backward = forward),
        );
    ).construct(.forward, .backward)
)
