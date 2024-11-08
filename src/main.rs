mod cli;
mod repl_helper;

use std::{
    io::{IsTerminal, Read},
    sync::{Arc, Mutex},
};

use kast::*;

fn run_repl<H: rustyline::Helper>(
    helper: H,
    mut handler: impl FnMut(String) -> eyre::Result<()>,
) -> eyre::Result<()> {
    let mut rustyline = rustyline::Editor::with_config(
        rustyline::Config::builder().auto_add_history(true).build(),
    )?;
    rustyline.set_helper(Some(helper));
    let is_tty = std::io::stdin().is_terminal();
    tracing::debug!("is tty: {is_tty:?}");
    loop {
        let s = match is_tty {
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
        if let Err(e) = handler(s) {
            println!("{e:?}");
        };
        if !is_tty {
            break;
        }
    }
    Ok(())
}

fn main() -> eyre::Result<()> {
    color_eyre::config::HookBuilder::new()
        .display_env_section(false)
        .display_location_section(false)
        .install()?;
    tracing_subscriber::fmt::init();
    let cli_args = cli::parse();
    match cli_args.command {
        cli::Command::Run { path } => {
            let mut kast = Kast::new();
            let value = kast.eval_source(
                SourceFile {
                    contents: std::fs::read_to_string(&path)?,
                    filename: path,
                },
                None,
            )?;
            match value {
                Value::Unit => {}
                _ => tracing::info!("evaluated to {value}"),
            }
        }
        cli::Command::ParseAst => {
            let syntax = ast::read_syntax(SourceFile {
                contents: std::fs::read_to_string(std_path().join("syntax.ks")).unwrap(),
                filename: "std/syntax.ks".into(),
            })?;
            tracing::trace!("{syntax:#?}");
            run_repl((), |contents| {
                let source = SourceFile {
                    contents,
                    filename: "<stdin>".into(),
                };
                let ast = ast::parse(&syntax, source)?;
                match ast {
                    None => println!("<nothing>"),
                    Some(ast) => println!("{ast:#}"),
                }
                Ok(())
            })?;
        }
        cli::Command::Repl => {
            let kast = Arc::new(Mutex::new(Kast::new()));
            let helper = repl_helper::Helper::new(kast.clone());
            run_repl(helper, |contents| {
                let source = SourceFile {
                    contents,
                    filename: "<stdin>".into(),
                };
                let value = kast.lock().unwrap().eval_source(source, None)?;
                println!("{} :: {}", value, value.ty());
                Ok(())
            })?;
        }
    }

    Ok(())
}
