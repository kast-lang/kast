use (import "./common.ks").*;

const Delim = newtype {
    .c1 :: Char,
    .c2 :: Option.t[Char],
    .raw :: String,
};

const new_delim = (s :: String) -> Delim => (
    if String.length(s) > 2 then (
        panic("at most 2 char delim please");
    );
    let c1 = String.at(s, 0);
    let c2 = if String.length(s) > 1 then (
        :Some String.at(s, 1)
    ) else (
        :None
    );
    { .c1, .c2, .raw = s }
);

const next_is_delim = (delim :: Delim) -> Bool => (
    let ctx = @current Context;
    let reader = &ctx.lexer^.reader;
    if delim.c2 is :Some c2 then (
        reader |> next_two_are(delim.c1, c2)
    ) else (
        reader |> next_is(delim.c1)
    )
);
# assumes we already checked that next_is_delim
const consume_delim = (delim :: Delim) -> Token.t => (
    let ctx = @current Context;
    let lexer = ctx.lexer;
    let reader = &mut lexer^.reader;
    let start = reader^.position;
    for _ in String.iter(delim.raw) do (
        reader |> Reader.advance;
    );
    {
        .shape = :Punct { .raw = delim.raw },
        .span = {
            .start,
            .end = reader^.position,
            .path = lexer^.source.path,
        },
    }
);

module:

const read_string_impl = (
    .delim :: Delim,
    .multiline :: Bool,
    .strip_indentation :: Bool,
) => with_return (
    let ctx = @current Context;
    let lexer = ctx.lexer;
    let reader = &mut lexer^.reader;
    if not next_is_delim(delim) then (
        return :None;
    );
    let start = reader^.position;
    let open_token = consume_delim(delim);
    let mut parts = ArrayList.new();
    let reset_raw_content_part = () => {
        .start = reader^.position,
    };
    let reset_content_part = () => {
        .start = reader^.position,
        .raw_parts = ArrayList.new(),
        .raw_content_part = reset_raw_content_part(),
        .contents = "",
    };
    let mut content_part = reset_content_part();
    let add_char = (c :: Char) => (
        content_part.contents += to_string(c);
    );

    let finish_raw_content_part = () => {
        let start = content_part.raw_content_part.start.string_encoding_index;
        let end = reader^.position.string_encoding_index;
        if start != end then (
            let part :: Token.RawStringPart = :Content {
                .raw = String.substring(reader^.contents, start, end - start),
                .span = {
                    .start = content_part.raw_content_part.start,
                    .end = reader^.position,
                    .path = lexer^.source.path,
                },
            };
            &mut content_part.raw_parts |> ArrayList.push_back(part);
        );
        content_part.raw_content_part = reset_raw_content_part();
    };

    let finish_content_part = () => with_return (
        finish_raw_content_part();
        if content_part.contents |> String.length == 0 then return;
        &mut parts
            |> ArrayList.push_back(
                :Content {
                    .raw_parts = content_part.raw_parts,
                    .span = {
                        .start = content_part.start,
                        .end = reader^.position,
                        .path = lexer^.source.path,
                    },
                    .raw = (
                        let start = content_part.start.string_encoding_index;
                        let end = reader^.position.string_encoding_index;
                        String.substring(reader^.contents, start, end - start)
                    ),
                    .contents = content_part.contents,
                }
            );
        content_part = reset_content_part();
    );
    let stripped_indentation = if strip_indentation then (
        let mut indentation = "";
        while Reader.peek(&reader^) is :Some c do (
            if c == '\n' then (
                indentation = ""
            ) else if Char.is_whitespace(c) then (
                indentation += to_string(c);
            ) else (
                break;
            );
            reader |> Reader.advance;
        );
        indentation
    ) else "";
    while Reader.peek(&reader^) is :Some c do (
        const outer_loop_body = @current std.LoopBody;
        if c == '\n' then (
            if not multiline then (
                break;
            );
            if strip_indentation then (
                add_char('\n');
                reader |> Reader.advance;
                for expected in String.iter(stripped_indentation) do (
                    if next_is_delim(delim) then break;
                    if &reader^ |> next_is('\n') then (
                        break;
                    );
                    if not &reader^ |> next_is(expected) then (
                        error_at_current_position("Expected more indentation in multiline string");
                    );
                    reader |> Reader.advance;
                );
                continue;
            );
        );
        if next_is_delim(delim) then break;
        if c == '\\' then (
            finish_raw_content_part();
            let escape_start = reader^.position;
            let c = match Reader.peek2(&reader^) with (
                | :Some c => c
                | :None => error_at_current_position("Expected escaped char")
            );
            if c == '(' then (
                # "hello, \(user.name)"
                finish_content_part();
                let open_token = {
                    .shape = :Punct { .raw = "\\(" },
                    .span = {
                        .start = reader^.position,
                        .end = (
                            let mut pos = reader^.position;
                            &mut pos |> Position.advance('\\');
                            &mut pos |> Position.advance('(');
                            pos
                        ),
                        .path = lexer^.source.path,
                    },
                };
                Reader.advance(reader);
                Reader.advance(reader);
                let span_start = reader^.position;
                let mut tokens = ArrayList.new();
                let mut bracket_balance :: Int32 = 0;
                @loop (
                    skip_whitespace();
                    let c = match Reader.peek(&reader^) with (
                        | :Some c => c
                        | :None => (
                            let span = {
                                .start = span_start,
                                .end = reader^.position,
                                .path = lexer^.source.path,
                            };
                            error_with_span(span, "Unfinished interpolation")
                        )
                    );
                    if c == ')' and bracket_balance == 0 then (
                        let close_token = {
                            .shape = :Punct { .raw = ")" },
                            .span = Span.single_char(
                                .position = reader^.position,
                                .char = :Some ')',
                                .path = lexer^.source.path,
                            ),
                        };
                        &mut parts
                            |> ArrayList.push_back(
                                :Interpolated {
                                    .tokens,
                                    .open = open_token,
                                    .close = close_token,
                                    .span = {
                                        .start = span_start,
                                        .end = reader^.position,
                                        .path = lexer^.source.path,
                                    }
                                }
                            );
                        Reader.advance(reader);
                        content_part = reset_content_part();
                        unwind (@binding outer_loop_body) ();
                    );
                    let token = ctx.next_token();
                    let token_raw = Token.raw(token);
                    if token_raw == "(" then (
                        bracket_balance += 1;
                    ) else if token_raw == ")" then (
                        bracket_balance -= 1;
                    );
                    &mut tokens |> ArrayList.push_back(token);
                )
            );
            Reader.advance(reader);
            let c = if c == '0' then (
                '\0'
            ) else if c == '\\' then (
                '\\'
            ) else if c == 'n' then (
                '\n'
            ) else if c == 'r' then (
                '\r'
            ) else if c == 'b' then (
                '\b'
            ) else if c == 't' then (
                '\t'
            ) else if c == 'f' then (
                '\f'
            ) else if c == '\'' then (
                '\''
            ) else if c == '"' then (
                '"'
            ) else if c == 'x' then (
                Reader.advance(reader);
                let { c1, c2 } = unwindable chars (
                    let error_position :: Position = with_return (
                        let mut pos = reader^.position;
                        let c1 = Reader.peek(&reader^)
                            |> Option.unwrap_or_else(
                                () => return pos
                            );
                        &mut pos |> Position.advance(c1);
                        let c2 = Reader.peek2(&reader^)
                            |> Option.unwrap_or_else(
                                () => return pos
                            );
                        &mut pos |> Position.advance(c2);
                        if not is_hex_digit(c1) or not is_hex_digit(c2) then (
                            return pos;
                        );
                        unwind chars { c1, c2 }
                    );
                    let span = {
                        .start = escape_start,
                        .end = error_position,
                        .path = lexer^.source.path,
                    };
                    error_with_span(span, "Expected 2 hex digits after '\\x'")
                );
                Reader.advance(reader);
                Reader.advance(reader);
                let c1 = Char.to_digit_radix(c1, 16);
                let c2 = Char.to_digit_radix(c2, 16);
                let code = c1 * 16 + c2;
                if code > 0x7f then (
                    let span = {
                        .start = escape_start,
                        .end = reader^.position,
                        .path = lexer^.source.path,
                    };
                    error_with_span(span, "Hex escaped char must be in range 0x00 to 0x7f");
                );
                add_char(Char.from_code(code));
                continue
            ) else if c == 'u' then (
                Reader.advance(reader);
                let c = match Reader.peek(&reader^) with (
                    | :Some c => c
                    | :None => expected("{")
                );
                if c != '{' then (
                    expected("{")
                );
                Reader.advance(reader);
                let hex_code_start = reader^.position;
                let hex_code = Reader.read_while(reader, is_hex_digit);
                let hex_code_span :: Span = {
                    .start = hex_code_start,
                    .end = reader^.position,
                    .path = lexer^.source.path,
                };
                let c = match Reader.peek(&reader^) with (
                    | :Some c => c
                    | :None => error_at_current_position("Expected }")
                );
                if c != '}' then (
                    error_at_current_position("Expected }")
                );
                Reader.advance(reader);
                if hex_code |> String.length == 0 then (
                    error_with_span(hex_code_span, "This escape must have at least 1 hex digit");
                );
                if hex_code |> String.length > 6 then (
                    error_with_span(hex_code_span, "This escape must have at most 6 hex digits");
                );
                let mut code = 0;
                for c in String.iter(hex_code) do (
                    code = code * 16 + Char.to_digit_radix(c, 16);
                );
                add_char(Char.from_code(code));
                continue
            ) else (
                error_at_current_position("Unexpected escape char " + to_string(c))
            );
            add_char(c);
            Reader.advance(reader);
            let escape_raw_part = :Escape {
                .raw = (
                    let start = escape_start.string_encoding_index;
                    let end = reader^.position.string_encoding_index;
                    String.substring(reader^.contents, start, end - start)
                ),
                .span = {
                    .start = escape_start,
                    .end = reader^.position,
                    .path = lexer^.source.path,
                }
            };
            &mut content_part.raw_parts |> ArrayList.push_back(escape_raw_part);
            content_part.raw_content_part = reset_raw_content_part();
            continue;
        );
        add_char(c);
        Reader.advance(reader);
    );
    finish_content_part();
    if &parts |> ArrayList.length == 0 then (
        &mut parts
            |> ArrayList.push_back(
                :Content {
                    .span = Span.empty(
                        .position = reader^.position,
                        .path = lexer^.source.path,
                    ),
                    .raw_parts = ArrayList.new(),
                    .raw = "",
                    .contents = "",
                }
            );
    );
    if not next_is_delim(delim) then (
        let span_so_far = {
            .start,
            .end = reader^.position,
            .path = lexer^.source.path,
        };
        error_with_span(span_so_far, "Unfinished string");
    );
    let close_token = consume_delim(delim);
    let raw = (
        let start = start.string_encoding_index;
        let end = reader^.position.string_encoding_index;
        String.substring(reader^.contents, start, end - start)
    );
    if &parts |> ArrayList.length == 1 then (
        if (&parts |> ArrayList.at(0))^ is :Content { .raw_parts, .contents, ... } then (
            return :Some :String {
                .raw,
                .raw_parts,
                .contents,
                .open = open_token,
                .close = close_token,
                .stripped_indentation,
            };
        );
    );
    :Some :InterpolatedString {
        .delimiter = delim.raw,
        .raw,
        .open = open_token,
        .parts,
        .close = close_token,
        .stripped_indentation,
    }
);

const read_string :: ReadFn = () => with_return (
    if read_string_impl(
        .delim = new_delim("''"),
        .multiline = true,
        .strip_indentation = true,
    ) is :Some token then (
        return :Some token;
    );
    if read_string_impl(
        .delim = new_delim("'"),
        .multiline = false,
        .strip_indentation = false,
    ) is :Some token then (
        return :Some token;
    );
    if read_string_impl(
        .delim = new_delim("\""),
        .multiline = false,
        .strip_indentation = false,
    ) is :Some token then (
        return :Some token;
    );
    :None
);
