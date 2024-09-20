mod lexer;

fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt::init();
    tracing::info!("Kast > Rust > Haskell");
    let mut rustyline = rustyline::DefaultEditor::with_config(
        rustyline::Config::builder().auto_add_history(true).build(),
    )?;
    loop {
        match rustyline.readline("> ") {
            Ok(line) => {
                let tokens: Result<Vec<lexer::SpannedToken>, lexer::Error> =
                    lexer::lex(lexer::SourceFile {
                        contents: line.chars().collect(),
                        filename: "<stdin>".into(),
                    })
                    .collect();
                let tokens: Vec<lexer::Token> = tokens?
                    .into_iter()
                    .map(|spanned_token| spanned_token.token)
                    .collect();
                tracing::info!("tokens: {tokens:#?}");
            }
            Err(rustyline::error::ReadlineError::Eof) => break,
            Err(e) => return Err(e.into()),
        }
    }
    Ok(())
}
