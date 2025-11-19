module:
const substring = (s :: string, start :: int32, len :: int32) -> string => cfg_if (
    | (native "==") (target.name, "interpreter") => (native "string.substring") (s, start, len)
);
const iter = (s :: string, f :: char -> ()) -> () => cfg_if (
    | (native "==") (target.name, "interpreter") => (native "string.iter") (s, f)
);
const iteri = (s :: string, f :: (int32, char) -> ()) => (
    let i = 0;
    iter (
        s,
        c => (
            f (i, c);
            # i = i + 1;
            i = (native "+") (i, 1);
        )
    )
);
const index_of = (c :: char, s :: string) -> int32 => (
    unwindable block (
        iteri (
            s,
            (i, c_at_i) => (
                if (native "==") (c, c_at_i) then (unwind block i) else ()
            )
        );
        (native "-") (0, 1)
    )
);
const last_index_of = (c :: char, s :: string) -> int32 => (
    let result = (native "-") (0, 1);
    iteri (
        s,
        (i, c_at_i) => (
            if (native "==") (c, c_at_i) then (result = i) else ()
        )
    );
    result
);
