module:
const length = (s :: string) -> int32 => cfg_if (
    | target.name == "interpreter" => (@native "string.length") s
);
const at = (s :: string, idx :: int32) -> char => cfg_if (
    | target.name == "interpreter" => (@native "string.at") (s, idx)
);
const substring = (s :: string, start :: int32, len :: int32) -> string => cfg_if (
    | target.name == "interpreter" => (@native "string.substring") (s, start, len)
);
const iter = (s :: string, f :: char -> ()) -> () => cfg_if (
    | target.name == "interpreter" => (@native "string.iter") (s, f)
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
                if c == c_at_i then (unwind block i) else ()
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
            if c == c_at_i then (result = i) else ()
        )
    );
    result
);
const lines = (s :: string, f :: string -> ()) -> () => (
    let start = 0;
    let endline = i => (
        let line = substring (s, start, (@native "-") (i, start));
        f line;
        start = i + 1;
    );
    iteri (
        s,
        (i, c) => (
            if c == '\n' then (
                endline i
            ) else ()
        )
    );
    endline (length s);
);
const split = (s :: string, sep :: char, f :: string -> ()) => (
    let start = 0;
    let perform_split = i => (
        let part = substring (s, start, i - start);
        f part;
        start = i + 1;
    );
    iteri (
        s,
        (i, c) => (
            if c == sep then (
                perform_split i
            ) else ()
        ),
    );
    perform_split (length s);
);
const split_once = (s :: string, sep :: char) -> (string, string) => (
    unwindable block (
        iteri (
            s,
            (i, c) => (
                if c == sep then (
                    unwind block (
                        substring (s, 0, i),
                        substring (s, i + 1, length s - i - 1)
                    )
                );
            ),
        );
        # TODO panic
        _
    )
);
const trim_matches = (s :: string, f :: char -> bool) -> string => (
    let len = length s;
    let start = 0;
    while start < len and at (s, start) |> f do (
        start += 1;
    );
    let end = len;
    while end > start and at (s, end - 1) |> f do (
        end -= 1;
    );
    substring (s, start, end - start)
);
const trim = s => trim_matches (s, Char.is_whitespace);

# replace all occurences of a string by a new string
const replace_all_owned = (s :: string, .old :: string, .new :: string) => with_return (
    # an empty `old` means we cannot replace
    if String.length old == 0 then return s;
    # an empty `s` means we cannot replace
    if String.length s == 0 then return s;
    # `s` smaller than `old` means we cannot replace
    if String.length s < String.length old then return s;
    if String.length s == String.length old then return (
        if s == old then (
            # `s` == `old` means replaced is just `new`
            new
        ) else (
            # `s` equal to `old` in size but not contents means we cannot replace
            s
        )
    );
    
    let end = (String.length s - String.length old + 1);
    let start :: int32 = 0;
    while start < end and String.substring (s, start, String.length old) != old do (
        start += 1
    );
    
    if start == end then (
        # `old` not found in `s`
        s
    ) else (
        # `old` found in `s`, replace with `new` and continue searching in remaining portion of `s`
        let rest = String.substring (
            s,
            start + String.length old,
            String.length s - start - String.length old
        );
        let replaced_rest = replace_all_owned (rest, .old, .new);
        
        String.substring (s, 0, start)
        + new
        + replaced_rest
    )
);

const FromString = [Self] newtype (
    .from_string :: string -> Self
);

impl int32 as FromString = (
    .from_string = s => cfg_if (
        | target.name == "interpreter" => (@native "parse") s
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
impl int64 as FromString = (
    .from_string = s => cfg_if (
        | target.name == "interpreter" => (@native "parse") s
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
impl float64 as FromString = (
    .from_string = s => cfg_if (
        | target.name == "interpreter" => (@native "parse") s
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
impl bool as FromString = (
    .from_string = s => if s == "true" then (
        true
    ) else if s == "false" then (
        false
    ) else (
        panic ("cannot parse '" + s + "' as bool")
    )
);

const ToString = [Self] newtype (
    .to_string :: Self -> string
);

impl int32 as ToString = (
    .to_string = num => cfg_if (
        | target.name == "interpreter" => (@native "to_string") num
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
impl int64 as ToString = (
    .to_string = num => cfg_if (
        | target.name == "interpreter" => (@native "to_string") num
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
impl float64 as ToString = (
    .to_string = num => cfg_if (
        | target.name == "interpreter" => (@native "to_string") num
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
impl bool as ToString = (
    .to_string = b => if b then "true" else "false"
);

const parse = [T] (s :: string) -> T => (
    (T as FromString).from_string s
);
const to_string = [T] (value :: T) -> string => (
    (T as ToString).to_string value
);
