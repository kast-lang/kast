module:
const length = (s :: string) -> int32 => cfg_if (
    | (native "==") (target.name, "interpreter") => (native "string.length") s
);
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
            i = i + 1;
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
        0 - 1
    )
);
const last_index_of = (c :: char, s :: string) -> int32 => (
    let result = 0 - 1;
    iteri (
        s,
        (i, c_at_i) => (
            if (native "==") (c, c_at_i) then (result = i) else ()
        )
    );
    result
);
const lines = (s :: string, f :: string -> ()) -> () => (
    let start = 0;
    let endline = i => (
        let line = substring (s, start, (native "-") (i, start));
        f line;
        start = i + 1;
    );
    iteri (
        s,
        (i, c) => (
            if (native "==") (c, '\n') then (
                endline i
            ) else ()
        )
    );
    endline (length s);
);
