// the git has been figured out
mod cli;
mod repl_helper;

use std::{
    io::{IsTerminal, Read},
    sync::{Arc, Mutex},
};

use kast::*;

fn run_repl(
    helper: impl repl_helper::AnyHelper,
    mut handler: impl FnMut(String) -> eyre::Result<()> + 'static,
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

    let log_level = {
        use tracing_subscriber::{filter, fmt, prelude::*, reload};
        let (filter, reload_handle) = reload::Layer::new(filter::LevelFilter::WARN);
        tracing_subscriber::registry()
            .with(filter)
            .with(fmt::Layer::default().with_writer(std::io::stderr))
            .init();
        reload_handle
    };
    let cli_args = cli::parse();
    let target_log_level = cli_args.log;
    let init_log_level = move || {
        log_level
            .modify(|filter| *filter = target_log_level)
            .unwrap();
    };

    match cli_args.command() {
        cli::Command::Compile {
            path,
            out_path,
            no_stdlib,
            target,
        } => {
            let mut kast = match no_stdlib {
                true => Kast::new_nostdlib(&path),
                false => Kast::new(&path),
            }
            .unwrap();
            init_log_level();
            let ir: Expr = kast.compile_file(path)?;
            let result: Box<dyn std::fmt::Display> = match target {
                cli::CompilationTarget::Ir => Box::new(ir),
                cli::CompilationTarget::JavaScript { engine } => {
                    let js = futures_lite::future::block_on(kast.transpile_to_javascript(
                        engine,
                        &ValueShape::Expr(Parc::new(ir)).into(),
                        javascript::ShowOptions::Pretty,
                    ))?;
                    Box::new(js)
                }
            };
            if let Some(out_path) = out_path {
                let mut writer = std::io::BufWriter::new(std::fs::File::create(out_path)?);
                use std::io::Write;
                write!(writer, "{result}")?;
            } else {
                print!("{result}");
            }
        }
        cli::Command::Run { path, no_stdlib } => {
            let mut kast = match no_stdlib {
                true => Kast::new_nostdlib(&path),
                false => Kast::new(&path),
            }
            .unwrap();
            init_log_level();
            let value = kast.eval_file(path)?;
            match value.clone().inferred() {
                Some(ValueShape::Unit) => {}
                _ => tracing::info!("evaluated to {value}"),
            }
        }
        cli::Command::ParseAst => {
            let syntax = ast::read_syntax(SourceFile {
                contents: std::fs::read_to_string(std_path().join("syntax.ks")).unwrap(),
                filename: "std/syntax.ks".into(),
            })?;
            tracing::trace!("{syntax:#?}");
            run_repl((), move |contents| {
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
        cli::Command::Repl {
            path,
            no_stdlib,
            prerun,
        } => {
            let kast = Arc::new(Mutex::new(
                match no_stdlib {
                    false => Kast::new("<stdin>"),
                    true => Kast::new_nostdlib("<stdin>"),
                }
                .unwrap(),
            ));
            init_log_level();
            if let Some(path) = path {
                let name = path.file_stem().unwrap().to_str().unwrap();
                let mut kast = kast.lock().unwrap();
                let value = kast.eval_file(&path).expect("Failed to eval file");
                let symbol = kast.new_symbol(
                    name,
                    Span {
                        start: Position::ZERO,
                        end: Position::ZERO,
                        filename: path.clone(),
                    },
                );
                kast.add_local(symbol, value);
            }
            {
                let prerun = prerun.unwrap_or(
                    "use std.prelude.*; with std.default_number_type_based_on_dot;".into(),
                );
                kast.lock()
                    .unwrap()
                    .eval_source::<Value>(
                        SourceFile {
                            contents: prerun,
                            filename: "<prerun>".into(),
                        },
                        Some(TypeShape::Unit.into()),
                    )
                    .expect("Failed to eval prerun");
            }
            let helper = repl_helper::Helper::new(kast.clone());
            run_repl(helper, move |contents| {
                let snapshot = kast::inference::global_state::snapshot();
                let source = SourceFile {
                    contents,
                    filename: "<stdin>".into(),
                };
                let place_ref: PlaceRef = match kast.lock().unwrap().eval_source(source, None) {
                    Ok(value) => value,
                    Err(e) => {
                        kast::inference::global_state::revert(snapshot);
                        return Err(e);
                    }
                };
                let place = place_ref.read()?;
                let ty = place.ty().unwrap_or_else(|| place_ref.place.ty.clone());
                match ty.inferred() {
                    Ok(TypeShape::Unit) => {}
                    _ => println!("{:#} :: {:#}", *place, ty),
                }
                Ok(())
            })?;
        }
    }

    Ok(())
}
