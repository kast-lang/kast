impl String as module = (
    module:

    const from_str = (s :: &str) -> String => @cfg (
        | target.name == "interpreter" => (@native "String.from_str")(s)
        | target.name == "c" => @native "String_from_StringView(\(s))"
        | target.name == "javascript" => @native "\(s)"
    );

    const as_str = (s :: &String) -> &str => @cfg (
        | target.name == "interpreter" => (@native "String.as_str")(s)
        | target.name == "c" => @native "String_as_StringView(\(s))"
        | target.name == "javascript" => @native "\(s)"
    );

    const length = (s :: &str) -> Int32 => @cfg (
        | target.name == "interpreter" => (@native "string.length")(s)
        | target.name == "c" => @native "String_length(\(s))"
        | target.name == "javascript" => (@native "Kast.String.length")(s)
    );
    const utf8_length = (s :: &str) -> Int32 => @cfg (
        | target.name == "interpreter" => (@native "string.length")(s)
        | target.name == "c" => @native "String_utf8_length(\(s))"
        | target.name == "javascript" => (@native "Kast.String.utf8_length")(s)
    );
    const at = (s :: &str, idx :: Int32) -> Char => @cfg (
        | target.name == "interpreter" => (@native "string.at")(s, idx)
        | target.name == "c" => @native "String_at(\(s), \(idx))"
        | target.name == "javascript" => (@native "Kast.String.at")(s, idx)
    );
    const substring = (s :: &str, start :: Int32, len :: Int32) -> &str => @cfg (
        | target.name == "interpreter" => (@native "string.substring")(s, start, len)
        | target.name == "c" => @native "String_substring(\(s), \(start), \(len))"
        | target.name == "javascript" => (@native "Kast.String.substring")(s, start, len)
    );
    const substring_from = (s :: &str, start :: Int32) -> &str => (
        substring(s, start, length(s) - start)
    );
    const strip_prefix = (s :: &str, .prefix :: &str) -> Option.t[&str] => (
        let prefix_len = String.length(prefix);
        if (
            String.length(s) >= prefix_len
            and String.substring(s, 0, prefix_len) == prefix
        ) then (
            :Some String.substring_from(s, prefix_len)
        ) else (
            :None
        )
    );
    const starts_with = (s :: &str, .prefix :: &str) -> Bool => (
        match strip_prefix(s, .prefix) with (
            | :Some _ => true
            | :None => false
        )
    );
    const iter = (s :: &str) -> std.iter.Iterable[Char] => @cfg (
        | target.name == "interpreter" => {
            .iter = f => (@native "string.iter")(s, f)
        }
        | target.name == "c" => {
            .iter = f => (
                let @"impl" :: fn (&str, Char -> ()) -> () = @native "String_iter";
                @"impl"(s, f);
            ),
        }
        | target.name == "javascript" => {
            .iter = f => (@native "Kast.String.iter")(s, f)
        }
    );
    const iteri = (s :: &str) -> std.iter.Iterable[type { Int32, Char }] => @cfg (
        | target.name == "interpreter" => {
            .iter = f => (@native "string.iteri")(s, (i, c) => f({ i, c }))
        }
        | target.name == "c" => {
            .iter = @move f => (
                let @"impl" :: fn (&str, (Int32, Char) -> ()) -> () = @native "String_iteri";
                @"impl"(s, @move (i, c) => f({ i, c }));
            ),
        }
        | target.name == "javascript" => {
            .iter = f => (@native "Kast.String.iteri")(s, f)
        }
    );
    const iteri_rev = (s :: &str) -> std.iter.Iterable[type { Int32, Char }] => @cfg (
        | target.name == "interpreter" => {
            .iter = f => (@native "string.iteri_rev")(s, (i, c) => f({ i, c }))
        }
        | target.name == "c" => {
            .iter = @move f => (
                let @"impl" :: fn (&str, (Int32, Char) -> ()) -> () = @native "String_iteri_rev";
                @"impl"(s, @move (i, c) => f({ i, c }));
            ),
        }
        | target.name == "javascript" => {
            .iter = f => (@native "Kast.String.iteri_rev")(s, f)
        }
    );

    const index_of = (s :: &str, c :: Char) -> Int32 => with_return (
        for { i, c_at_i } in iteri(s) do (
            if c == c_at_i then (
                return i;
            );
        );
        -1
    );
    const last_index_of = (s :: &str, c :: Char) -> Int32 => (
        let mut result = -1;
        for { i, c_at_i } in iteri(s) do (
            if c == c_at_i then (
                result = i;
            );
        );
        result
    );
    const split = (s :: &str, sep :: Char) -> std.iter.Iterable[&str] => {
        .iter = @move f => (
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
    const split_once = (s :: &str, sep :: Char) -> { &str, &str } => with_return (
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
    const trim_matches = (s :: &str, f :: Char -> Bool) -> &str => (
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
    const replace_all_owned = (s :: &str, .old :: &str, .new :: &str) -> String => with_return (
        # an empty `old` means we cannot replace
        if length(old) == 0 then return from_str(s);
        # an empty `s` means we cannot replace
        if length(s) == 0 then return from_str(s);
        # `s` smaller than `old` means we cannot replace
        if length(s) < length(old) then return from_str(s);
        if length(s) == length(old) then return from_str(
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
            from_str(s)
        ) else (
            # `old` found in `s`, replace with `new` and continue searching in remaining portion of `s`
            let rest = substring(
                s,
                start + length(old),
                length(s) - start - length(old)
            );
            let replaced_rest = replace_all_owned(rest, .old, .new);
            
            StringBuilder.build(() => (
                StringBuilder.add_str(substring(s, 0, start));
                StringBuilder.add_str(new);
                StringBuilder.add_String(replaced_rest);
            ))
        )
    );
    
    # find if string contains another string
    const contains = (s :: &str, search :: &str) -> Bool => with_return (
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
        s :: &str, f :: Char -> Bool
    ) -> std.Option.t[type { Int32, Char }] => with_return (
        iteri(s).iter({ idx, c } => if f(c) then return :Some { idx, c });
        :None
    );
    
    const to_ascii_lowercase = (s :: &str) -> String => (
        let next_alphabet = find_match(s, Char.is_ascii_uppercase);
        match next_alphabet with (
            | :Some { i, c } => StringBuilder.build(() => (
                StringBuilder.add_str(substring(s, 0, i));
                StringBuilder.add_String(to_string(Char.to_ascii_lowercase(c)));
                StringBuilder.add_String(to_ascii_lowercase(substring(s, i + 1, length(s) - i - 1)));
            ))
            | :None => from_str(s)
        )
    );
    
    const to_ascii_uppercase = (s :: &str) -> String => (
        let next_alphabet = find_match(s, Char.is_ascii_lowercase);
        match next_alphabet with (
            | :Some { i, c } => StringBuilder.build(() => (
                StringBuilder.add_str(substring(s, 0, i));
                StringBuilder.add_String(to_string(Char.to_ascii_uppercase(c)));
                StringBuilder.add_String(to_ascii_uppercase(substring(s, i + 1, length(s) - i - 1)));
            ))
            | :None => from_str(s)
        )
    );

    const is_whitespace = (s :: &str) -> Bool => (
        iter(s) |> std.iter.all(Char.is_whitespace)
    );
    
    const Parse = [Self] newtype {
        .parse :: &str -> Self
    };
    
    impl Int32 as Parse = {
        .parse = s => @cfg (
            | target.name == "interpreter" => (@native "parse")(s)
            | target.name == "c" => @native "Int32_from_String(\(s))"
            | target.name == "javascript" => (@native "Kast.parse.Int32")(s)
        )
    };
    impl UInt32 as Parse = {
        .parse = s => @cfg (
            | target.name == "interpreter" => (@native "parse")(s)
            | target.name == "c" => @native "Int32_from_String(\(s))"
            | target.name == "javascript" => (@native "Kast.parse.Int32")(s)
        )
    };
    impl Int64 as Parse = {
        .parse = s => @cfg (
            | target.name == "interpreter" => (@native "parse")(s)
            | target.name == "c" => @native "Int64_from_String(\(s))"
            | target.name == "javascript" => (@native "Kast.parse.Int64")(s)
        )
    };
    impl UInt64 as Parse = {
        .parse = s => @cfg (
            | target.name == "interpreter" => (@native "parse")(s)
            | target.name == "c" => @native "Int64_from_String(\(s))"
            | target.name == "javascript" => (@native "Kast.parse.Int64")(s)
        )
    };
    impl Float32 as Parse = {
        .parse = s => @cfg (
            | target.name == "interpreter" => (@native "parse")(s)
            | target.name == "c" => @native "Float64_from_String(\(s))"
            | target.name == "javascript" => (@native "Kast.parse.Float64")(s)
        )
    };
    impl Float64 as Parse = {
        .parse = s => @cfg (
            | target.name == "interpreter" => (@native "parse")(s)
            | target.name == "c" => @native "Float64_from_String(\(s))"
            | target.name == "javascript" => (@native "Kast.parse.Float64")(s)
        )
    };
    impl Bool as Parse = {
        .parse = s => if s == "true" then (
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

    impl String as ToString = {
        .to_string = s => s,
    };

    impl &str as ToString = {
        .to_string = String.from_str,
    };
    
    impl Char as ToString = {
        .to_string = c => @cfg (
            | target.name == "interpreter" => (@native "to_string")(c)
            | target.name == "c" => @native "Char_to_String(\(c))"
            | target.name == "javascript" => (@native "Kast.String.to_string")(c)
        )
    };
    impl Int32 as ToString = {
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string")(num)
            | target.name == "c" => @native "Int32_to_String(\(num))"
            | target.name == "javascript" => (@native "Kast.String.to_string")(num)
        )
    };
    impl UInt32 as ToString = {
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string")(num)
            | target.name == "c" => @native "Int32_to_String(\(num))"
            | target.name == "javascript" => (@native "Kast.String.to_string")(num)
        )
    };
    impl Int64 as ToString = {
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string")(num)
            | target.name == "c" => @native "Int64_to_String(\(num))"
            | target.name == "javascript" => (@native "Kast.String.to_string")(num)
        )
    };
    impl UInt64 as ToString = {
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string")(num)
            | target.name == "c" => @native "Int64_to_String(\(num))"
            | target.name == "javascript" => (@native "Kast.String.to_string")(num)
        )
    };
    impl Float32 as ToString = {
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string")(num)
            | target.name == "c" => @native "Float32_to_String(\(num))"
            | target.name == "javascript" => (@native "Kast.String.to_string")(num)
        )
    };
    impl Float64 as ToString = {
        .to_string = num => @cfg (
            | target.name == "interpreter" => (@native "to_string")(num)
            | target.name == "c" => @native "Float64_to_String(\(num))"
            | target.name == "javascript" => (@native "Kast.String.to_string")(num)
        )
    };
    impl Bool as ToString = {
        .to_string = b => String.from_str(if b then "true" else "false")
    };
    
    const parse = [T] (s :: &str) -> T => (
        (T as Parse).parse(s)
    );
    const to_string = [T] (value :: T) -> String => (
        (T as ToString).to_string(value)
    );

    const escape_contents = (s :: &str, .delimiter :: &str) -> String => (
        StringBuilder.build(() => (
            let mut result = String.from_str("");
            for c in String.iter(s) do (
                if c == '\\' then (
                    StringBuilder.add_str("\\\\");
                    continue;
                );
                if c == '\n' then (
                    StringBuilder.add_str("\\n");
                    continue;
                );
                if c == '\r' then (
                    StringBuilder.add_str("\\r");
                    continue;
                );
                if c == '\b' then (
                    StringBuilder.add_str("\\b");
                    continue;
                );
                if c == '\f' then (
                    StringBuilder.add_str("\\f");
                    continue;
                );
                if c == '\t' then (
                    StringBuilder.add_str("\\t");
                    continue;
                );
                if Char.is_ascii_control(c) then (
                    let code = Char.code(c);
                    if code <= 0x7f then (
                        let c1 = code / 16;
                        let c2 = code % 16;
                        StringBuilder.add_str("\\x");
                        StringBuilder.add_String(to_string(Char.from_digit_radix(c1, 16)));
                        StringBuilder.add_String(to_string(Char.from_digit_radix(c2, 16)));
                    ) else (
                        StringBuilder.add_str("\\u{");
                        let mut p = 1;
                        while p * 16 <= code do (
                            p *= 16;
                        );
                        let mut code = code;
                        while p > 0 do (
                            let digit = code / p;
                            StringBuilder.add_String(
                                to_string(Char.from_digit_radix(digit, 16))
                            );
                            code = code - digit * p;
                            p /= 16;
                        );
                        StringBuilder.add_str("}");
                    );
                    continue;
                );
                let cs = to_string(c);
                if &cs |> as_str == delimiter then (
                    StringBuilder.add_str("\\");
                );
                StringBuilder.add_String(cs);
            );
        ))
    );

    const escape_with = (s :: &str, .delimiter :: &str) -> String => (
        StringBuilder.build(() => (
            StringBuilder.add_str(delimiter);
            StringBuilder.add_String(escape_contents(s, .delimiter));
            StringBuilder.add_str(delimiter);
        ))
    );

    const escape = s => escape_with(s, .delimiter = "\"");
);

const StringBuilder = (
    module:

    const CtxT = newtype {
        .result :: String,
    };

    const Ctx = @context CtxT;

    const init = () -> CtxT => {
        .result = String.from_str(""),
    };

    const build = (f :: () -> ()) => (
        with Ctx = init();
        f();
        (@current Ctx).result
    );

    const add_str = (s :: &str) => (
        add_String(String.from_str(s));
    );

    const add_String = (s :: String) => (
        let result = &mut (@current Ctx).result;
        @cfg (
            | target.name == "interpreter" => (
                result^ = (@native "+")(result^, s);
            )
            | target.name == "javascript" => (
                result^ = @native "\(result^) + \(s)";
            )
            | target.name == "c" => (
                result^ = @native "String_concat(\(result^), \(s))";
            )
        );
    );
);
