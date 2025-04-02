use std::path::PathBuf;

#[derive(clap::Subcommand)]
pub enum Command {
    ParseAst {
        path: Option<PathBuf>,
        #[clap(short, long, default_value_t = false)]
        interactive: bool,
    },
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
#[clap(args_conflicts_with_subcommands = true)]
pub struct Args {
    #[clap(long, default_value = "info")]
    pub log: tracing_subscriber::filter::LevelFilter,
    pub path: Option<PathBuf>,
    #[clap(subcommand)]
    pub command: Option<Command>,
}

impl Args {
    pub fn command(self) -> Command {
        match self.command {
            Some(command) => command,
            None => match self.path {
                Some(path) => Command::Run {
                    path,
                    no_stdlib: false,
                },
                None => Command::Repl {
                    path: None,
                    no_stdlib: false,
                },
            },
        }
    }
}

pub fn parse() -> Args {
    if cfg!(target_arch = "wasm32") {
        return Args {
            log: tracing_subscriber::filter::LevelFilter::WARN,
            path: None,
            command: None,
        };
    }
    clap::Parser::parse()
}
