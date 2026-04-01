impl String as module = (
    module:
    const length = (s :: String) -> Int32 => @cfg (
        | target.name == "interpreter" => (@native "string.length")(s)
        | target.name == "javascript" => (@native "Kast.String.length")(s)
    );
    const at = (s :: String, idx :: Int32) -> Char => @cfg (
        | target.name == "interpreter" => (@native "string.at")(s, idx)
        | target.name == "javascript" => (@native "Kast.String.at")(s, idx)
    );
    const substring = (s :: String, start :: Int32, len :: Int32) -> String => @cfg (
        | target.name == "interpreter" => (@native "string.substring")(s, start, len)
        | target.name == "javascript" => (@native "Kast.String.substring")(s, start, len)
    );
    const iter = (s :: String) -> std.iter.Iterable[Char] => @cfg (
        | target.name == "interpreter" => {
            .iter = f => (@native "string.iter")(s, f)
        }
        | target.name == "javascript" => {
            .iter = f => (@native "Kast.String.iter")(s, f)
        }
    );
    const iteri = (s :: String) -> std.iter.Iterable[type { Int32, Char }] => @cfg (
        | target.name == "interpreter" => {
            .iter = f => (@native "string.iteri")(s, (i, c) => f({ i, c }))
        }
        | target.name == "javascript" => {
            .iter = f => (@native "Kast.String.iteri")(s, f)
        }
    );
    const iteri_rev = (s :: String) -> std.iter.Iterable[type { Int32, Char }] => @cfg (
        | target.name == "interpreter" => {
            .iter = f => (@native "string.iteri_rev")(s, (i, c) => f({ i, c }))
        }
        | target.name == "javascript" => {
            .iter = f => (@native "Kast.String.iteri_rev")(s, f)
        }
    );
    const pop_back = (s :: &mut String) -> Char => with_return (
        for { i, c } in iteri_rev(s^) do (
            s^ = substring(s^, 0, i);
            return c;
        );
        panic("empty string")
    );
    const index_of = (s :: String, c :: Char) -> Int32 => with_return (
        for { i, c_at_i } in iteri(s) do (
            if c == c_at_i then (
                return i;
            );
        );
        -1
    );
    const last_index_of = (s :: String, c :: Char) -> Int32 => (
        let mut result = -1;
        for { i, c_at_i } in iteri(s) do (
            if c == c_at_i then (
                result = i;
            );
        );
        result
    );
    const split = (s :: String, sep :: Char) -> std.iter.Iterable[String] => {
        .iter = f => (
            let mut start = 0;
            let perform_split = i => (
                let part = substring(s, start, i - start);
                f(part);
                start = i + 1;
            );
            for { i, c } in iteri(s) do (
                if c == sep then (
                    perform_split(i);
                );
            );
            perform_split(length(s));
        )
    };
    const lines = s => split(s, '\n');
    const split_once = (s :: String, sep :: Char) -> { String, String } => with_return (
        for { i, c } in iteri(s) do (
            if c == sep then (
                return {
                    substring(s, 0, i),
                    substring(s, i + 1, length(s) - i - 1),
                }
            );
        );
        panic("split_once separator not found")
    );
    const trim_matches = (s :: String, f :: Char -> Bool) -> String => (
        let len = length(s);
        let mut start = 0;
        while start < len and at(s, start) |> f do (
            start += 1;
        );
        let mut end = len;
        while end > start and at(s, end - 1) |> f do (
            end -= 1;
        );
        substring(s, start, end - start)
    );
    const trim = s => trim_matches(s, Char.is_whitespace);
    
    # replace all occurences of a string by a new string
    const replace_all_owned = (s :: String, .old :: String, .new :: String) => with_return (
        # an empty `old` means we cannot replace
        if length(old) == 0 then return s;
        # an empty `s` means we cannot replace
        if length(s) == 0 then return s;
        # `s` smaller than `old` means we cannot replace
        if length(s) < length(old) then return s;
        if length(s) == length(old) then return (
            if s == old then (
                # `s` == `old` means replaced is just `new`
                new
            ) else (
                # `s` equal to `old` in size but not contents means we cannot replace
                s
            )
        );
        
        let end = (length(s) - length(old) + 1);
        let mut start :: Int32 = 0;
        while start < end and substring(s, start, length(old)) != old do (
            start += 1
        );
        
        if start == end then (
            # `old` not found in `s`
            s
        ) else (
            # `old` found in `s`, replace with `new` and continue searching in remaining portion of `s`
            let rest = substring(
                s,
                start + length(old),
                length(s) - start - length(old)
            );
            let replaced_rest = replace_all_owned(rest, .old, .new);
            
            substring(s, 0, start)
            + new
            + replaced_rest
        )
    );
    
    # find if string contains another string
    const contains = (s :: String, search :: String) -> Bool => with_return (
        # an empty `search` is not contained
        if length(search) == 0 then return false;
        # an empty `s` contains nothing
        if length(s) == 0 then return false;
        # `s` smaller than `search` means it cannot be contained
        if length(s) < length(search) then return false;
        if length(s) == length(search) then return (
            if s == search then (
                # `s` == `search` means `s` contains `search`
                true
            ) else (
                # `s` equal to `search` in size but not contents means `s` does not contains `search`
                false
            )
        );
        
        let end = (length(s) - length(search) + 1);
        let mut start :: Int32 = 0;
        while start < end and substring(s, start, length(search)) != search do (
            start += 1
        );
        
        # if loop ended before end, `s` contains `search`
        start != end
    );
    
    const find_match = (
        s :: String, f :: Char -> Bool
    ) -> std.Option.t[type { Int32, Char }] => with_return (
        iteri(s).iter({ idx, c } => if f(c) then return :Some { idx, c });
        :None
    );
    
    const to_ascii_lowercase = (s :: String) -> String => (
        let next_alphabet = find_match(s, Char.is_ascii_uppercase);
        match next_alphabet with (
            | :Some { i, c } => (
                substring(s, 0, i)
                + to_string(Char.to_ascii_lowercase(c))
                + to_ascii_lowercase(substring(s, i + 1, length(s) - i - 1))
            )
            | :None => s
        )
    );
    
    const to_ascii_uppercase = (s :: String) -> String => (
        let next_alphabet = find_match(s, Char.is_ascii_lowercase);
        match next_alphabet with (
            | :Some { i, c } => (
                substring(s, 0, i)
                + to_string(Char.to_ascii_uppercase(c))
                + to_ascii_uppercase(substring(s, i + 1, length(s) - i - 1))
            )
            | :None => s
        )
    );

    const is_whitespace = (s :: String) -> Bool => (
        iter(s) |> std.iter.all(Char.is_whitespace)
    );
    
    const FromString = [Self] newtype {
        .from_string :: String -> Self
    };
    
    impl Int32 as FromString = {
        .from_string = s => @cfg (
            | target.name == "interpreter" => (@native "parse")(s)
            | target.name == "ocaml" => @native "@natives.todo()"
            | target.name == "javascript" => (@native "Kast.parse.Int32")(s)
        )
    };
    impl Int64 as FromString = {
        .from_string = s => @cfg (
            | target.name == "interpreter" => (@native "parse")(s)
            | target.name == "ocaml" => @native "@natives.todo()"
            | target.name == "javascript" => (@native "Kast.parse.Int64")(s)
        )
    };
    impl Float64 as FromString = {
        .from_string = s => @cfg (
            | target.name == "interpreter" => (@native "parse")(s)
            | target.name == "ocaml" => @native "@natives.todo()"
            | target.name == "javascript" => (@native "Kast.parse.Float64")(s)
        )
    };
    impl Bool as FromString = {
        .from_string = s => if s == "true" then (
            true
        ) else if s == "false" then (
            false
        ) else (
            panic("cannot parse '" + s + "' as bool")
        )
    };
    
    const ToString = [Self] newtype {
        .to_string :: Self -> String
    };
    
    impl Char as ToString = {
        .to_string = c => @cfg (
            | target.name == "interpreter" => (@native "to_string")(c)
            | target.name == "javascript" => (@native "Kast.String.to_string")(c)
        )
    };
    impl Int32 as ToString = {
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string")(num)
            | target.name == "ocaml" => @native "@natives.todo()"
            | target.name == "javascript" => (@native "Kast.String.to_string")(num)
        )
    };
    impl Int64 as ToString = {
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string")(num)
            | target.name == "ocaml" => @native "@natives.todo()"
            | target.name == "javascript" => (@native "Kast.String.to_string")(num)
        )
    };
    impl Float64 as ToString = {
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string")(num)
            | target.name == "ocaml" => @native "@natives.todo()"
            | target.name == "javascript" => (@native "Kast.String.to_string")(num)
        )
    };
    impl Bool as ToString = {
        .to_string = b => if b then "true" else "false"
    };
    
    const parse = [T] (s :: String) -> T => (
        (T as FromString).from_string(s)
    );
    const to_string = [T] (value :: T) -> String => (
        (T as ToString).to_string(value)
    );

    const escape_contents = (s :: String, .delimiter :: String) -> String => (
        let mut result = "";
        for c in String.iter(s) do (
            if c == '\\' then (
                result += "\\\\";
                continue;
            );
            if c == '\n' then (
                result += "\\n";
                continue;
            );
            if c == '\r' then (
                result += "\\r";
                continue;
            );
            if c == '\b' then (
                result += "\\b";
                continue;
            );
            if c == '\f' then (
                result += "\\f";
                continue;
            );
            if c == '\t' then (
                result += "\\t";
                continue;
            );
            if Char.is_ascii_control(c) then (
                let code = Char.code(c);
                if code <= 0x7f then (
                    let c1 = code / 16;
                    let c2 = code % 16;
                    result += "\\x";
                    result += to_string(Char.from_digit_radix(c1, 16));
                    result += to_string(Char.from_digit_radix(c2, 16));
                ) else (
                    result += "\\u{";
                    let mut p = 1;
                    while p * 16 <= code do (
                        p *= 16;
                    );
                    let mut code = code;
                    while p > 0 do (
                        let digit = code / p;
                        result += to_string(Char.from_digit_radix(digit, 16));
                        code = code - digit * p;
                        p /= 16;
                    );
                    result += "}";
                );
                continue;
            );
            let cs = to_string(c);
            if cs == delimiter then (
                result += "\\";
            );
            result += cs;
        );
        result
    );

    const escape_with = (s :: String, .delimiter :: String) -> String => (
        delimiter + escape_contents(s, .delimiter) + delimiter
    );

    const escape = s => escape_with(s, .delimiter = "\"");
);
