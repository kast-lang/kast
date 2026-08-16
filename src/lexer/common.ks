module:

use (import "../reader.ks").*;
use (import "../source.ks").*;
use (import "../span.ks").*;
use (import "../position.ks").*;
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../token.ks").*;

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

const Lexer = newtype {
    .reader :: Reader,
    .source :: Source,
};

const ContextT = newtype {
    .lexer :: &mut Lexer,
    .reader :: &mut Reader,
    .next_token :: () -> Token.t,
};
const Context = @context ContextT;

const TokenStart = @context Position;

const error_with_span = [T] (span, message) -> T => (
    let diagnostic = {
        .severity = :Error,
        .source = :Lexer,
        .span,
        .message = () => (@current Output).write(message),
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);
const error_at_current_position = [T] message -> T => (
    let ctx = @current Context;
    let span = Span.single_char(
        .position = ctx.reader^.position,
        .char = Reader.peek(&ctx.reader^),
        .path = ctx.lexer^.source.path,
    );
    error_with_span(span, message)
);
const error_token = [T] message -> T => (
    let ctx = @current Context;
    let span = {
        .start = @current TokenStart,
        .end = ctx.reader^.position,
        .path = ctx.lexer^.source.path,
    };
    error_with_span(span, message)
);

const expected = [T] expected -> T => (
    let ctx = @current Context;
    let got = match Reader.peek(&ctx.reader^) with (
        | :Some c => to_string(c)
        | :None => "<eof>"
    );
    error_at_current_position("Expected " + expected + ", got " + got)
);

const unexpected_char = [T] (message :: String) -> T => (
    let ctx = @current Context;
    let lexer = ctx.lexer;
    let char = Reader.peek(&lexer^.reader);
    let span = Span.single_char(
        .position = lexer^.reader.position,
        .char,
        .path = lexer^.source.path,
    );
    Reader.advance(&mut lexer^.reader);
    error_with_span(span, message)
);

const skip_whitespace = () => (
    let ctx = @current Context;
    while Reader.peek(&ctx.reader^) is :Some c do (
        if Char.is_whitespace(c) then (
            Reader.advance(ctx.reader);
        ) else (
            break;
        )
    );
);
# Lexer will try multiple read fns, and the first returning :Some is the token
const ReadFn = type (() -> Option.t[Token.Shape.t]);
