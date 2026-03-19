use (import "./source.ks").*;
use (import "./lexer.ks").*;

let mut lexer = Lexer.new(Source.read_file("tests/hello.ks"));

loop (
    let token = &lexer |> Lexer.peek;
    dbg.print(token);
    if token.shape is :Eof then break;
    Lexer.advance(&mut lexer);
);