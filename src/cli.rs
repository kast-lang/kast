use std::path::PathBuf;

#[derive(clap::Subcommand)]
pub enum Command {
    ParseAst,
    Repl {
        path: Option<PathBuf>,
        #[clap(long)]
        no_stdlib: bool,
        #[clap(long)]
        prerun: Option<String>,
    },
    Run {
        path: PathBuf,
        #[clap(long)]
        no_stdlib: bool,
    },
    Compile {
        path: PathBuf,
        #[clap(long)]
        out_path: Option<PathBuf>,
        #[clap(long)]
        no_stdlib: bool,
        /// TODO maybe subcommand?
        #[clap(long)]
        target: CompilationTarget,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum CompilationTarget {
    JavaScriptNode,
    Ir,
}

impl std::str::FromStr for CompilationTarget {
    type Err = eyre::Report;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "js" | "javascript" => Self::JavaScriptNode,
            "ir" => Self::Ir,
            _ => eyre::bail!("{s:?} is unknown target"),
        })
    }
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
                    prerun: None,
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
