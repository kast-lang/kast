use std::path::PathBuf;

#[derive(clap::Subcommand)]
pub enum Command {
    ParseAst,
    Repl { path: Option<PathBuf> },
    Run { path: PathBuf },
}

#[derive(clap::Parser)]
pub struct Args {
    #[clap(subcommand)]
    pub command: Command,
}

pub fn parse() -> Args {
    if cfg!(target_arch = "wasm32") {
        return Args {
            command: Command::Repl { path: None },
        };
    }
    clap::Parser::parse()
}
