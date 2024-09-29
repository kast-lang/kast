use eyre::{eyre, Context as _};
use kast_ast as ast;
use kast_util::*;
use std::{
    io::{IsTerminal, Read},
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

mod cli;
mod compiler;
mod id;
mod inference;
mod interpreter;
mod ir;
mod ty;
mod value;

use id::*;
use ir::*;
use kast_ast::{Ast, Token};
use ty::*;
use value::*;

struct Kast {
    compiler: compiler::State,
    interpreter: interpreter::State,
}

impl Kast {
    fn new() -> Self {
        Self {
            compiler: compiler::State::new(),
            interpreter: interpreter::State::new(),
        }
    }
}

fn std_path() -> PathBuf {
    match std::env::var_os("KAST_STD") {
        Some(path) => path.into(),
        None => match option_env!("CARGO_MANIFEST_DIR") {
            Some(path) => Path::new(path).join("std"),
            None => panic!("kast standard library not found"),
        },
    }
}

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
            let syntax = ast::read_syntax(SourceFile {
                contents: std::fs::read_to_string(std_path().join("syntax.ks")).unwrap(),
                filename: "std/syntax.ks".into(),
            })?;
            let kast = Arc::new(Mutex::new(Kast::new()));
            let helper = {
                struct Helper(Arc<Mutex<Kast>>);
                struct CompletionCandidate {
                    display: String,
                    replacement: String,
                }
                impl rustyline::completion::Candidate for CompletionCandidate {
                    fn display(&self) -> &str {
                        &self.display
                    }
                    fn replacement(&self) -> &str {
                        &self.replacement
                    }
                }
                impl rustyline::completion::Completer for Helper {
                    type Candidate = CompletionCandidate;
                    fn complete(
                        &self,
                        line: &str,
                        pos: usize,
                        _ctx: &rustyline::Context<'_>,
                    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
                        let start = line[..pos]
                            .rfind(|c| kast_ast::is_punctuation(c) || c.is_whitespace())
                            .map(|i| i + 1)
                            .unwrap_or(0);
                        let part = &line[start..pos];
                        let kast = self.0.lock().unwrap();
                        let completions = kast
                            .interpreter
                            .autocomplete(part)
                            .map(|candidate| CompletionCandidate {
                                display: format!("{} :: {}", candidate.name, candidate.ty),
                                replacement: candidate.name,
                            })
                            .collect();
                        Ok((start, completions))
                    }
                }
                impl rustyline::hint::Hinter for Helper {
                    type Hint = String;
                }
                impl rustyline::highlight::Highlighter for Helper {}
                impl rustyline::validate::Validator for Helper {}
                impl rustyline::Helper for Helper {}
                Helper(kast.clone())
            };
            run_repl(helper, |contents| {
                let source = SourceFile {
                    contents,
                    filename: "<stdin>".into(),
                };
                let ast = ast::parse(&syntax, source)?;
                let Some(ast) = ast else {
                    // empty line
                    return Ok(());
                };
                let value = kast.lock().unwrap().eval_ast(&ast, None)?;
                println!("{} :: {}", value, value.ty());
                Ok(())
            })?;
        }
    }

    Ok(())
}
