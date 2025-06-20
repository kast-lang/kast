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
        #[clap(long)]
        target: Option<CompilationTarget>,
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

#[derive(Debug, Clone)]
pub enum CompilationTarget {
    #[cfg(feature = "javascript")]
    JavaScript {
        engine: kast::javascript::JavaScriptEngineType,
    },
    Ir,
}

impl std::str::FromStr for CompilationTarget {
    type Err = eyre::Report;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            #[cfg(feature = "javascript")]
            "js" | "javascript" | "js-node" | "javascript-node" => Self::JavaScript {
                engine: kast::javascript::JavaScriptEngineType::Node,
            },
            #[cfg(feature = "javascript")]
            "js-web" | "javascript-web" => Self::JavaScript {
                engine: kast::javascript::JavaScriptEngineType::Browser,
            },
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
                    target: None,
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
