use (import "./span.ks").*;
use (import "./source.ks").*;
use (import "./reader.ks").*;
use (import "./token.ks").*;
use (import "./error.ks").*;

module:

const Lexer = newtype {
    .peeked :: Option.t[Token],
    .source :: Source,
    .reader :: Reader,
};

impl Lexer as module = (
    module:
    
    const new = (source :: Source) -> Lexer => (
        let mut lexer = {
            .peeked = :None,
            .source,
            .reader = Reader.new(source.contents),
        };
        advance(&mut lexer);
        lexer
    );
    
    const peek = (lexer :: &Lexer) -> Token => (
        lexer^.peeked |> Option.unwrap
    );
    
    const advance = (lexer :: &mut Lexer) => (
        lexer^.peeked = :Some read_next(lexer);
    );
    
    const read_next = (lexer :: &mut Lexer) -> Token => with_return (
        const ReadFn = type (&mut Reader -> Option.t[TokenShape]);
        const skip_whitespace :: ReadFn = reader => (
            while Reader.peek(&reader^) is :Some c do (
                if Char.is_whitespace(c) then (
                    Reader.advance(reader);
                ) else (
                    break;
                )
            );
            :None
        );
        const read_eof :: ReadFn = reader => (
            match Reader.peek(&reader^) with (
                | :Some _ => :None
                | :None => :Some :Eof
            )
        );
        const read_punct :: ReadFn = reader => with_return (
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
                String.index_of(c, "@(){}[]&^$;\\,") >= 0
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
        
        const read_ident :: ReadFn = reader => with_return (
            let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
            if not is_ident_start(c) then return :None;
            let start = reader^.position.index;
            Reader.advance(reader);
            reader |> Reader.read_while(Char.is_ascii_alphanumeric);
            let end = reader^.position.index;
            :Some :Ident {
                .raw = String.substring(reader^.contents, start, end - start),
            }
        );
        
        const read_string_with_delim = (reader, delim :: Char) => with_return (
            let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
            if c != delim then return :None;
            let start = reader^.position.index;
            Reader.advance(reader);
            reader |> Reader.read_while(c => c != delim);
            let c = Reader.peek(&reader^)
                |> Option.unwrap_or_else(
                    () => (
                        panic("Unfinished string (eof)")
                    )
                );
            if c != delim then panic("Unfinished string");
            Reader.advance(reader);
            let end = reader^.position.index;
            :Some :String {
                .raw = String.substring(reader^.contents, start, end - start),
            }
        );
        
        const read_string :: ReadFn = reader => (
            read_string_with_delim(reader, '\'')
                |> Option.or_else(() => read_string_with_delim(reader, '"'))
        );
        
        const read_fns :: ArrayList.t[ReadFn] = (
            let mut read_fns = ArrayList.new();
            &mut read_fns |> ArrayList.push_back(skip_whitespace);
            &mut read_fns |> ArrayList.push_back(read_eof);
            &mut read_fns |> ArrayList.push_back(read_punct);
            &mut read_fns |> ArrayList.push_back(read_ident);
            &mut read_fns |> ArrayList.push_back(read_string);
            read_fns
        );
        
        for &read_fn in ArrayList.iter(&read_fns) do (
            let start = { ...lexer^.reader.position };
            if read_fn(&mut lexer^.reader) is :Some shape then (
                let end = lexer^.reader.position;
                return {
                    .shape,
                    .span = { .start, .end, .uri = lexer^.source.uri },
                };
            );
        );
        unexpected_char(lexer, "None of the read fns returned Some")
    );
    
    const unexpected_char = (lexer :: &mut Lexer, message :: String) -> Token => (
        let char = Reader.peek(&lexer^.reader) |> Option.unwrap;
        let span = Span.single_char(
            .position = lexer^.reader.position,
            .char,
            .uri = lexer^.source.uri,
        );
        Error.report(span, message);
        Reader.advance(&mut lexer^.reader);
        {
            .shape = :Error { .raw = to_string(char) },
            .span,
        }
    );
);
