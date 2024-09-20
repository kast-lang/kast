use std::io::Read;

mod lexer;

fn main() {
    let mut s = String::new();
    std::io::stdin().lock().read_to_string(&mut s).unwrap();
    let tokens: Result<Vec<lexer::SpannedToken>, lexer::Error> = lexer::lex(lexer::SourceFile {
        contents: s.chars().collect(),
        filename: "<stdin>".into(),
    })
    .collect();
    let tokens: Vec<lexer::Token> = tokens
        .unwrap()
        .into_iter()
        .map(|spanned_token| spanned_token.token)
        .collect();
    dbg!(tokens);
    println!("Rust > Haskell");
}
