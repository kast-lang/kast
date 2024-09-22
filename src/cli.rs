#[derive(clap::Subcommand)]
pub enum Command {
    ParseAst,
}

#[derive(clap::Parser)]
pub struct Args {
    #[clap(subcommand)]
    pub command: Command,
}

pub fn parse() -> Args {
    clap::Parser::parse()
}
