use (import "./source.ks").*;
use (import "./reader.ks").*;
use (import "./token.ks").*;

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
        const actual :: ReadFn = reader => (
            let raw = reader |> Reader.read_while(c => not Char.is_whitespace(c));
            :Some :Raw raw
        );
        const read_fns :: ArrayList.t[ReadFn] = (
            let mut read_fns = ArrayList.new();
            &mut read_fns |> ArrayList.push_back(skip_whitespace);
            &mut read_fns |> ArrayList.push_back(read_eof);
            &mut read_fns |> ArrayList.push_back(actual);
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
        panic("None of the read fns returned Some")
    );
);
