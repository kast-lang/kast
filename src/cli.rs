use std::path::PathBuf;

#[derive(clap::Subcommand)]
pub enum Command {
    ParseAst,
    Repl {
        path: Option<PathBuf>,
        #[clap(long)]
        no_stdlib: bool,
    },
    Run {
        path: PathBuf,
        #[clap(long)]
        no_stdlib: bool,
    },
}

#[derive(clap::Parser)]
pub struct Args {
    #[clap(subcommand)]
    pub command: Command,
}

pub fn parse() -> Args {
    if cfg!(target_arch = "wasm32") {
        return Args {
            command: Command::Repl {
                path: None,
                no_stdlib: false,
            },
        };
    }
    clap::Parser::parse()
}
