use (import "./span.ks").*;
use (import "./source.ks").*;
use (import "./reader.ks").*;
use (import "./token.ks").*;
use (import "./error.ks").*;

const is_hex_digit = (c :: Char) -> Bool => (
    ('0' <= c and c <= '9')
    or ('a' <= c and c <= 'f')
    or ('A' <= c and c <= 'F')
);

const next_is = (reader :: &Reader, c :: Char) -> Bool => (
    match reader |> Reader.peek with (
        | :Some peek => c == peek
        | :None => false
    )
);

const next_two_are = (reader :: &Reader, c1 :: Char, c2 :: Char) -> Bool => with_return (
    match reader |> Reader.peek with (
        | :Some c => if c != c1 then return false
        | :None => return false
    );
    match reader |> Reader.peek2 with (
        | :Some c => if c != c2 then return false
        | :None => return false
    );
    true
);

const panic = "NO PANIC";

module:

const Lexer = newtype {
    .source :: Source,
    .reader :: Reader,
};

impl Lexer as module = (
    module:
    
    const new = (source :: Source) -> Lexer => {
        .source,
        .reader = Reader.new(source.contents),
    };
    
    const next = (lexer :: &mut Lexer) -> Token.t => with_return (
        const ReadFn = type (&mut Lexer -> Option.t[Token.Shape.t]);
        const skip_whitespace :: ReadFn = lexer => (
            let reader = &mut lexer^.reader;
            while Reader.peek(&reader^) is :Some c do (
                if Char.is_whitespace(c) then (
                    Reader.advance(reader);
                ) else (
                    break;
                )
            );
            :None
        );
        const read_eof :: ReadFn = lexer => (
            let reader = &mut lexer^.reader;
            match Reader.peek(&reader^) with (
                | :Some _ => :None
                | :None => :Some :Eof
            )
        );
        const read_punct :: ReadFn = lexer => with_return (
            let reader = &mut lexer^.reader;
            const is_punct = (c :: Char) -> Bool => (
                if c == '_' or c == '\'' or c == '"' then (
                    false
                ) else if Char.is_whitespace(c) then (
                    false
                ) else if Char.is_ascii_alphanumeric(c) then (
                    false
                ) else (
                    true
                )
            );
            const is_single_punct = (c :: Char) -> Bool => (
                "@(){}[]&^$;\\," |> String.index_of(c) >= 0
            );
            let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
            if not is_punct(c) then return :None;
            let start = reader^.position.index;
            Reader.advance(reader);
            if not is_single_punct(c) then (
                reader |> Reader.read_while(c => is_punct(c) and not is_single_punct(c));
            );
            let end = reader^.position.index;
            :Some :Punct {
                .raw = String.substring(reader^.contents, start, end - start),
            }
        );
        
        const is_ident_start = (c :: Char) -> Bool => (
            c == '_' or Char.is_ascii_alpha(c)
        );
        
        const read_ident :: ReadFn = lexer => with_return (
            let reader = &mut lexer^.reader;
            let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
            if not is_ident_start(c) then return :None;
            let start = reader^.position.index;
            Reader.advance(reader);
            reader |> Reader.read_while(c => Char.is_ascii_alphanumeric(c) or c == '_');
            let end = reader^.position.index;
            let raw = String.substring(reader^.contents, start, end - start);
            :Some :Ident {
                .raw,
                .name = raw,
            }
        );
        
        const read_string_with_delim = (lexer, delim :: Char) => with_return (
            let reader = &mut lexer^.reader;
            let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
            if c != delim then return :None;
            let start = reader^.position.index;
            let error = [T] message -> T => (
                let span = Span.single_char(
                    .position = reader^.position,
                    .char = Reader.peek(&reader^),
                    .uri = lexer^.source.uri,
                );
                Error.report(span, message);
                let end = reader^.position.index;
                return :Some :Error { .raw = String.substring(reader^.contents, start, end - start) }
            );
            let expected = [T] expected -> T => (
                let got = match Reader.peek(&reader^) with (
                    | :Some c => to_string(c)
                    | :None => "<eof>"
                );
                error("Expected " + expected + ", got " + got)
            );
            Reader.advance(reader);
            let mut contents = "";
            let add_char = (c :: Char) => (
                contents += to_string(c);
            );
            while Reader.peek(&reader^) is :Some c do (
                if c == delim then break;
                if c == '\\' then (
                    Reader.advance(reader);
                    let c = match Reader.peek(&reader^) with (
                        | :Some c => c
                        | :None => error("Expected escaped char")
                    );
                    # TODO match instead
                    let c = if c == '(' then (
                        error("TODO interpolation")
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
                    
                    ) else if c == '\'' then (
                        '\''
                    
                    ) else if c == '"' then (
                        '"'
                    
                    ) else if c == 'x' then (
                        Reader.advance(reader);
                        let c1 = Reader.peek(&reader^)
                            |> Option.unwrap_or_else(
                                () => (
                                    error("Expected 2 hex digits after '\\x'")
                                )
                            );
                        let c2 = Reader.peek2(&reader^)
                            |> Option.unwrap_or_else(
                                () => (
                                    error("Expected 2 hex digits after '\\x'")
                                )
                            );
                        if not is_hex_digit(c1) or not is_hex_digit(c2) then (
                            error("Expected 2 hex digits after '\\x'");
                        );
                        Reader.advance(reader);
                        Reader.advance(reader);
                        let c1 = Char.to_digit_radix(c1, 16);
                        let c2 = Char.to_digit_radix(c2, 16);
                        let code = c1 * 16 + c2;
                        if code > 0x7f then (
                            error("Hex escaped char must be in range 0x00 to 0x7f");
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
                        let hex_code = Reader.read_while(reader, is_hex_digit);
                        let c = match Reader.peek(&reader^) with (
                            | :Some c => c
                            | :None => error("Expected }")
                        );
                        if c != '}' then (
                            error("Expected }")
                        );
                        Reader.advance(reader);
                        if hex_code |> String.length == 0 then (
                            error("This escape must have at least 1 hex digit");
                        );
                        if hex_code |> String.length > 6 then (
                            error("This escape must have at most 6 hex digits");
                        );
                        let mut code = 0;
                        for c in String.iter(hex_code) do (
                            code = code * 16 + Char.to_digit_radix(c, 16);
                        );
                        add_char(Char.from_code(code));
                        continue
                    ) else (
                        error("Unexpected escape char " + to_string(c))
                    );
                    add_char(c);
                    Reader.advance(reader);
                    continue;
                );
                add_char(c);
                Reader.advance(reader);
            );
            let c = Reader.peek(&reader^)
                |> Option.unwrap_or_else(
                    () => (
                        error("Unfinished string (eof)")
                    )
                );
            if c != delim then error("Unfinished string");
            Reader.advance(reader);
            let end = reader^.position.index;
            :Some :String {
                .delimeter = to_string(delim),
                .raw = String.substring(reader^.contents, start, end - start),
                .contents,
            }
        );
        
        const read_string :: ReadFn = lexer => (
            let reader = &mut lexer^.reader;
            read_string_with_delim(lexer, '\'')
                |> Option.or_else(() => read_string_with_delim(lexer, '"'))
        );
        
        const read_hex_number :: ReadFn = lexer => with_return (
            let reader = &mut lexer^.reader;
            let start = reader^.position.index;
            if not next_two_are(&reader^, '0', 'x') then (
                return :None;
            );
            Reader.advance(reader);
            Reader.advance(reader);
            reader |> Reader.read_while(is_hex_digit);
            let end = reader^.position.index;
            :Some :Number {
                .raw = String.substring(reader^.contents, start, end - start),
            }
        );
        
        const read_number :: ReadFn = lexer => with_return (
            let reader = &mut lexer^.reader;
            let start = reader^.position.index;
            let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
            if not Char.is_ascii_digit(c) then return :None;
            let seen_dot = match Reader.prev(&reader^) with (
                | :Some c => c == '.'
                | :None => false
            );
            Reader.read_while(reader, Char.is_ascii_digit);
            match Reader.peek(&reader^) with (
                | :Some c => (
                    let digit_after_dot = match Reader.peek2(&reader^) with (
                        | :Some c => Char.is_ascii_digit(c)
                        | :None => false
                    );
                    if not seen_dot and c == '.' and digit_after_dot then (
                        Reader.advance(reader);
                        Reader.read_while(reader, Char.is_ascii_digit);
                    )
                )
                | :None => ()
            );
            let end = reader^.position.index;
            :Some :Number {
                .raw = String.substring(reader^.contents, start, end - start),
            }
        );
        
        const read_line_comment :: ReadFn = lexer => with_return (
            let reader = &mut lexer^.reader;
            if not next_is(&reader^, '#') then (
                return :None;
            );
            let start = reader^.position.index;
            reader |> Reader.read_while(c => c != '\n');
            let end = reader^.position.index;
            :Some :Comment {
                .raw = String.substring(reader^.contents, start, end - start),
                .ty = :Line,
            }
        );
        
        const read_block_comment :: ReadFn = lexer => with_return (
            let reader = &mut lexer^.reader;
            if not next_two_are(&reader^, '(', '#') then (
                return :None
            );
            let start = reader^.position.index;
            let mut nest_depth :: Int32 = 0;
            loop (
                if next_two_are(&reader^, '(', '#') then (
                    Reader.advance(reader);
                    Reader.advance(reader);
                    nest_depth += 1;
                    continue;
                );
                if next_two_are(&reader^, '#', ')') then (
                    nest_depth -= 1;
                    Reader.advance(reader);
                    Reader.advance(reader);
                    if nest_depth == 0 then break;
                    continue;
                );
                Reader.advance(reader);
            );
            let end = reader^.position.index;
            :Some :Comment {
                .raw = String.substring(reader^.contents, start, end - start),
                .ty = :Line,
            }
        );

        const read_raw_keyword :: ReadFn = lexer => with_return (
            let reader = &mut lexer^.reader;
            if not &reader^ |> next_is('@') then (
                return :None;
            );
            let start = reader^.position.index;
            reader |> Reader.advance;
            let _ = read_ident(lexer);
            let end = reader^.position.index;
            :Some :Punct { 
                .raw = String.substring(reader^.contents, start, end - start),
            }
        );
        
        const read_fns :: ArrayList.t[ReadFn] = (
            let mut read_fns = ArrayList.new();
            &mut read_fns |> ArrayList.push_back(skip_whitespace);
            &mut read_fns |> ArrayList.push_back(read_eof);
            &mut read_fns |> ArrayList.push_back(read_line_comment);
            &mut read_fns |> ArrayList.push_back(read_block_comment);
            &mut read_fns |> ArrayList.push_back(read_raw_keyword);
            &mut read_fns |> ArrayList.push_back(read_punct);
            &mut read_fns |> ArrayList.push_back(read_ident);
            &mut read_fns |> ArrayList.push_back(read_string);
            &mut read_fns |> ArrayList.push_back(read_hex_number);
            &mut read_fns |> ArrayList.push_back(read_number);
            read_fns
        );
        
        for &read_fn in ArrayList.iter(&read_fns) do (
            let start = lexer^.reader.position;
            if read_fn(lexer) is :Some shape then (
                let end = lexer^.reader.position;
                return {
                    .shape,
                    .span = { .start, .end, .uri = lexer^.source.uri },
                };
            );
        );
        unexpected_char(lexer, "None of the read fns returned Some")
    );
    
    const unexpected_char = (lexer :: &mut Lexer, message :: String) -> Token.t => (
        let char = Reader.peek(&lexer^.reader);
        let span = Span.single_char(
            .position = lexer^.reader.position,
            .char,
            .uri = lexer^.source.uri,
        );
        Error.report(span, message);
        Reader.advance(&mut lexer^.reader);
        {
            .shape = :Error {
                .raw = match char with (
                    | :Some char => to_string(char)
                    | :None => ""
                ),
            },
            .span,
        }
    );
);
