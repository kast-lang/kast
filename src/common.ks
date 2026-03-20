module:

const is_printable = (c :: Char) -> Bool => (
    let code = Char.code(c);
    0x20 <= code and code <= 0xffff
);

const escape_string_contents = (s :: String, .delimeter :: String) -> String => (
    let mut result = "";
    for c in String.iter(s) do (
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
        if c == '\t' then (
            result += "\\t";
            continue;
        );
        if not is_printable(c) then (
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
        if cs == delimeter then (
            result += "\\";
        );
        result += cs;
    );
    result
);

const escape_string = (s :: String, .delimeter :: String) -> String => (
    delimeter + escape_string_contents(s, .delimeter) + delimeter
);