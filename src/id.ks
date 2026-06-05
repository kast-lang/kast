module:

const Id = newtype {
    .raw :: Int32,
};

const IdGen = newtype {
    .next_id :: Int32,
};

impl IdGen as module = (
    module:

    const new = () -> IdGen => {
        .next_id = 0,
    };
);

const IdGenCtx = @context IdGen;

impl Id as module = (
    module:

    const compare = (a :: Id, b :: Id) -> std.cmp.Ordering => (
        std.cmp.default_compare(a.raw, b.raw)
    );

    const gen = () -> Id => (
        let next_id = &mut (@current IdGenCtx).next_id;
        let raw = next_id^;
        next_id^ += 1;
        { .raw }
    );
);
