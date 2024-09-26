use std::{
    io::{IsTerminal, Read},
    path::{Path, PathBuf},
};

#[macro_use]
mod error;
mod ast;
mod lexer;
mod syntax;

mod cli;
mod peek2;
mod source;
mod tuple;

use source::*;

fn std_path() -> PathBuf {
    match std::env::var_os("KAST_STD") {
        Some(path) => path.into(),
        None => match option_env!("CARGO_MANIFEST_DIR") {
            Some(path) => Path::new(path).join("std"),
            None => panic!("kast standard library not found"),
        },
    }
}

fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt::init();
    let cli_args = cli::parse();
    match cli_args.command {
        cli::Command::ParseAst => {
            let mut rustyline = rustyline::DefaultEditor::with_config(
                rustyline::Config::builder().auto_add_history(true).build(),
            )?;

            let syntax = ast::read_syntax(SourceFile {
                contents: std::fs::read_to_string(std_path().join("syntax.ks")).unwrap(),
                filename: "std/syntax.ks".into(),
            })?;
            tracing::trace!("{syntax:#?}");

            let is_tty = std::io::stdin().is_terminal();
            tracing::debug!("is tty: {is_tty:?}");

            loop {
                let contents = match is_tty {
                    true => match rustyline.readline("> ") {
                        Ok(line) => line,
                        Err(rustyline::error::ReadlineError::Eof) => break,
                        Err(e) => return Err(e.into()),
                    },
                    false => {
                        let mut contents = String::new();
                        std::io::stdin()
                            .lock()
                            .read_to_string(&mut contents)
                            .unwrap();
                        contents
                    }
                };
                let source = SourceFile {
                    contents,
                    filename: "<stdin>".into(),
                };
                let values = ast::parse(&syntax, source)?;
                if values.is_empty() {
                    println!("<nothing>");
                }
                for (index, value) in values.iter().enumerate() {
                    if values.len() != 1 {
                        print!("value #{index}: ");
                    }
                    println!("{value:#}");
                }
                if !is_tty {
                    break;
                }
            }
        }
    }

    Ok(())
}
