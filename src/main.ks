use (import "./output.ks").*;
use (import "./source.ks").*;
use (import "./lexer.ks").*;
use (import "./token.ks").*;

let mut lexer = Lexer.new(Source.read_file("tests/hello.ks"));

with Output = stdout();
loop (
    let token = &lexer |> Lexer.peek;
    token |> Token.print;
    (@current Output).write("\n");
    # dbg.print(token);
    if token.shape is :Eof then break;
    Lexer.advance(&mut lexer);
);