impl Char as module = (
    module:
    const is_whitespace = (c :: Char) -> Bool => (
        c == ' ' or c == '\n' or c == '\t'
    );
    const is_uppercase = (c :: Char) -> Bool => (
        code(c) >= (@comptime code('A')) and code(c) <= (@comptime code('Z'))
    );
    const is_lowercase = (c :: Char) -> Bool => (
        code(c) >= (@comptime code('a')) and code(c) <= (@comptime code('z'))
    );
    const to_uppercase = (c :: Char) -> Char => (
        if is_lowercase(c) then (
            code(c) - (@comptime (Char.code('a') - Char.code('A')))
                |> from_code
        ) else (
            c
        )
    );
    const to_lowercase = (c :: Char) -> Char => (
        if is_uppercase(c) then (
            code(c) + (@comptime (Char.code('a') - Char.code('A')))
                |> from_code
        ) else (
            c
        )
    );
    const code = (c :: Char) -> UInt32 => @cfg (
        | target.name == "interpreter" => (@native "char.code")(c)
        | target.name == "javascript" => (@native "Kast.Char.code")(c)
    );
    const from_code = (code :: UInt32) -> Char => @cfg (
        | target.name == "interpreter" => (@native "char.from_code")(code)
        | target.name == "javascript" => (@native "Kast.Char.from_code")(code)
    );
    const to_digit_radix = (c :: Char, radix :: UInt32) -> UInt32 => (
        let code = code(c);
        let digit = if '0' <= c and c <= '9' then (
            code - (@comptime Char.code('0'))
        ) else if 'a' <= c and c <= 'z' then (
            code - (@comptime Char.code('a')) + 10
        ) else if 'A' <= c and c <= 'Z' then (
            code - (@comptime Char.code('A')) + 10
        ) else (
            panic("char is not digit")
        );
        if radix < 2 or digit >= radix then (
            panic("digit >= radix")
        );
        digit
    );
    const to_digit = c => to_digit_radix(c, 10);
    const from_digit_radix = (digit :: UInt32, radix :: UInt32) -> Char => (
        if radix < 2 or digit >= radix then (
            panic("digit >= radix")
        );
        if digit < 10 then (
            from_code(digit + (@comptime Char.code('0')))
        ) else (
            from_code(digit - 10 + (@comptime Char.code('a')))
        )
    );
    const from_digit = digit => from_digit_radix(digit, 10);
);
