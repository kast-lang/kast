const std = (
    module:

    impl syntax (a == b) = `(
        @native "\($a) === \($b)"
    );
    impl syntax (a < b) = `(
        @native "\($a) < \($b)"
    );
    impl syntax (a + b) = `(
        @native "\($a) + \($b)"
    );
    impl syntax (a += b) = `(
        let _ref = &mut $a;
        _ref^ = _ref^ + $b;
    );
    impl syntax (a - b) = `(
        @native "\($a) - \($b)"
    );
    impl syntax (a -= b) = `(
        let _ref = &mut $a;
        _ref^ = _ref^ - $b;
    );

    # const rem = [T](a :: T, b :: T) -> T => (@native "Kast.op.rem")(a, b);

    impl syntax (a % b) = `(
        # rem($a, $b)
        @native "\($a) % \($b)"
    );


    impl syntax (if cond then then_case) = `(
        if $cond then $then_case else ()
    );
    impl syntax (for pattern in iterable do body) = `(
        $iterable.iter($pattern => $body)
    );
    impl syntax (a..b) = `(
        std.range.range($a, $b)
    );

    const std = @current_scope;

    const Int32 = @native "Int32";

    const dbg = (
        module:
        const print = [T] (x :: T) => (
            @native "console.log(\(x))";
        )
    );

    const range = (
        module:
        const range = (start :: Int32, end :: Int32) => (
            let forward = consumer => (
                let mut i = start;
                unwindable block (
                    @loop (
                        if i < end then (
                            consumer(i);
                            i = i + 1;
                        ) else unwind block ()
                    )
                );
            );
            { .iter = forward }
        );
    );
);

# ACTUAL CODE

let mut sum = 0;

let is_prime = x => (
    unwindable block (
        for i in 2..x do (
            if x % i == 0 then (
                unwind block false;
            )
        );
        true
    )
);

for x in 2..30000 do (
    if is_prime(x) then (
        sum += x;
    );
);
std.dbg.print(sum);
