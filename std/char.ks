module:
let is_whitespace = (c :: char) -> bool => (
    c == ' ' or c == '\n' or c == '\t'
);
let code = (c :: char) -> uint32 => cfg_if (
    | target.name == "interpreter" => (@native "char.code") c
);
let from_code = (code :: uint32) -> char => cfg_if (
    | target.name == "interpreter" => (@native "char.from_code") code
);
let code_0 = code '0';
let code_a = code 'a';
let code_A = code 'A';
let to_digit_radix = (c :: char, radix :: uint32) -> uint32 => (
    let code = Char.code c;
    let digit = if '0' <= c and c <= '9' then (
        code - Char.code_0
    ) else if 'a' <= c and c <= 'z' then (
        code - Char.code_a + 10
    ) else if 'A' <= c and c <= 'Z' then (
        code - Char.code_A + 10
    ) else (
        panic "char is not digit"
    );
    if radix < 2 or digit >= radix then (
        panic "digit >= radix";
    );
    digit
);
let to_digit = c => to_digit_radix (c, 10);
let from_digit_radix = (digit :: uint32, radix :: uint32) -> char => (
    if radix < 2 or digit >= radix then (
        panic "digit >= radix";
    );
    if digit < 10 then (
        Char.from_code (digit + code_0)
    ) else (
        Char.from_code (digit - 10 + code_a)
    )
);
let from_digit = digit => from_digit_radix (digit, 10);
