module:

const Lazy = (
    module:

    const t = [T] newtype (
        | :Deferred (() -> T)
        | :Calculating
        | :Calculated T
    );

    const new = [T] (f :: () -> T) -> Lazy.t[T] => (
        let mut captured_context = @context;
        let f = @move () => (
            let &@context = &mut captured_context;
            f()
        );
        :Deferred f
    );

    const force = [T] (lazy :: &Lazy.t[T]) -> &T => (
        let lazy :: &mut Lazy.t[T] = @native "\(lazy)";
        match lazy^ with (
            | :Deferred f => (
                lazy^ = :Calculating;
                lazy^ = :Calculated f();
                let :Calculated ref value = lazy^;
                value
            )
            | :Calculating => (
                # TODO good errors
                panic("recursive lazy forced")
            )
            | :Calculated ref value => value
        )
    );
);
