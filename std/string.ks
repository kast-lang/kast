impl String as module = (
    module:
    const length = (s :: String) -> Int32 => @cfg (
        | target.name == "interpreter" => (@native "string.length") s
    );
    const at = (s :: String, idx :: Int32) -> Char => @cfg (
        | target.name == "interpreter" => (@native "string.at") (s, idx)
    );
    const substring = (s :: String, start :: Int32, len :: Int32) -> String => @cfg (
        | target.name == "interpreter" => (@native "string.substring") (s, start, len)
    );
    const iter = (s :: String, f :: Char -> ()) -> () => @cfg (
        | target.name == "interpreter" => (@native "string.iter") (s, f)
    );
    const iteri = (s :: String, f :: (Int32, Char) -> ()) => (
        let mut i = 0;
        iter (
            s,
            c => (
                f (i, c);
                i = i + 1;
            )
        )
    );
    const index_of = (c :: Char, s :: String) -> Int32 => (
        unwindable block (
            iteri (
                s,
                (i, c_at_i) => (
                    if c == c_at_i then (unwind block i) else ()
                )
            );
            -1
        )
    );
    const last_index_of = (c :: Char, s :: String) -> Int32 => (
        let mut result = -1;
        iteri (
            s,
            (i, c_at_i) => (
                if c == c_at_i then (result = i) else ()
            )
        );
        result
    );
    const lines = (s :: String, f :: String -> ()) -> () => (
        let mut start = 0;
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
    const split = (s :: String, sep :: Char, f :: String -> ()) => (
        let mut start = 0;
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
    const split_once = (s :: String, sep :: Char) -> (String, String) => (
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
    const trim_matches = (s :: String, f :: Char -> Bool) -> String => (
        let len = length s;
        let mut start = 0;
        while start < len and at (s, start) |> f do (
            start += 1;
        );
        let mut end = len;
        while end > start and at (s, end - 1) |> f do (
            end -= 1;
        );
        substring (s, start, end - start)
    );
    const trim = s => trim_matches (s, Char.is_whitespace);
    
    # replace all occurences of a string by a new string
    const replace_all_owned = (s :: String, .old :: String, .new :: String) => with_return (
        # an empty `old` means we cannot replace
        if length old == 0 then return s;
        # an empty `s` means we cannot replace
        if length s == 0 then return s;
        # `s` smaller than `old` means we cannot replace
        if length s < length old then return s;
        if length s == length old then return (
            if s == old then (
                # `s` == `old` means replaced is just `new`
                new
            ) else (
                # `s` equal to `old` in size but not contents means we cannot replace
                s
            )
        );
        
        let end = (length s - length old + 1);
        let mut start :: Int32 = 0;
        while start < end and substring (s, start, length old) != old do (
            start += 1
        );
        
        if start == end then (
            # `old` not found in `s`
            s
        ) else (
            # `old` found in `s`, replace with `new` and continue searching in remaining portion of `s`
            let rest = substring (
                s,
                start + length old,
                length s - start - length old
            );
            let replaced_rest = replace_all_owned (rest, .old, .new);
            
            substring (s, 0, start)
            + new
            + replaced_rest
        )
    );
    
    # find if string contains another string
    const contains = (s :: String, search :: String) -> Bool => with_return (
        # an empty `search` is not contained
        if length search == 0 then return false;
        # an empty `s` contains nothing
        if length s == 0 then return false;
        # `s` smaller than `search` means it cannot be contained
        if length s < length search then return false;
        if length s == length search then return (
            if s == search then (
                # `s` == `search` means `s` contains `search`
                true
            ) else (
                # `s` equal to `search` in size but not contents means `s` does not contains `search`
                false
            )
        );
        
        let end = (length s - length search + 1);
        let mut start :: Int32 = 0;
        while start < end and substring (s, start, length search) != search do (
            start += 1
        );
        
        # if loop ended before end, `s` contains `search`
        start != end
    );
    
    const FromString = [Self] newtype (
        .from_string :: String -> Self
    );
    
    impl Int32 as FromString = (
        .from_string = s => @cfg (
            | target.name == "interpreter" => (@native "parse") s
            | target.name == "ocaml" => @native "@natives.todo()"
        )
    );
    impl Int64 as FromString = (
        .from_string = s => @cfg (
            | target.name == "interpreter" => (@native "parse") s
            | target.name == "ocaml" => @native "@natives.todo()"
        )
    );
    impl Float64 as FromString = (
        .from_string = s => @cfg (
            | target.name == "interpreter" => (@native "parse") s
            | target.name == "ocaml" => @native "@natives.todo()"
        )
    );
    impl Bool as FromString = (
        .from_string = s => if s == "true" then (
            true
        ) else if s == "false" then (
            false
        ) else (
            panic ("cannot parse '" + s + "' as bool")
        )
    );
    
    const ToString = [Self] newtype (
        .to_string :: Self -> String
    );
    
    impl Int32 as ToString = (
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string") num
            | target.name == "ocaml" => @native "@natives.todo()"
        )
    );
    impl Int64 as ToString = (
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string") num
            | target.name == "ocaml" => @native "@natives.todo()"
        )
    );
    impl Float64 as ToString = (
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string") num
            | target.name == "ocaml" => @native "@natives.todo()"
        )
    );
    impl Bool as ToString = (
        .to_string = b => if b then "true" else "false"
    );
    
    const parse = [T] (s :: String) -> T => (
        (T as FromString).from_string s
    );
    const to_string = [T] (value :: T) -> String => (
        (T as ToString).to_string value
    );
);
