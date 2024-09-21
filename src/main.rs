#[macro_use]
mod error;
mod ast;
mod lexer;
mod syntax;

fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt::init();
    tracing::info!("Kast > Rust > Haskell");
    let mut rustyline = rustyline::DefaultEditor::with_config(
        rustyline::Config::builder().auto_add_history(true).build(),
    )?;

    let syntax = ast::read_syntax(lexer::SourceFile {
        contents: std::fs::read_to_string("std/syntax.ks")
            .unwrap()
            .chars()
            .collect(),
        filename: "std/syntax.ks".into(),
    });
    tracing::info!("{syntax:#?}");

    loop {
        match rustyline.readline("> ") {
            Ok(line) => {
                let source = lexer::SourceFile {
                    contents: line.chars().collect(),
                    filename: "<stdin>".into(),
                };
                let ast = ast::parse(&syntax, source).unwrap();
                tracing::info!("{ast:#}");
            }
            Err(rustyline::error::ReadlineError::Eof) => break,
            Err(e) => return Err(e.into()),
        }
    }
    Ok(())
}
