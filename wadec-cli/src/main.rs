use clap::Parser;
use patharg::InputArg;
use std::error::Error;
use std::process::exit;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// The module to decode. If not provided or is '-', read from
    /// standard input.
    #[arg(default_value_t)]
    pub input: InputArg,

    /// Enable verbose output, including a debug representation of
    /// the decoded module.
    #[arg(long, default_value_t = false)]
    pub verbose: bool,
}

fn main() {
    let cli = Cli::parse();
    let input = cli.input.open().unwrap_or_else(|e| abort(&cli, e));
    let module = wadec::decode(input).unwrap_or_else(|e| abort(&cli, e));

    if cli.verbose {
        println!("{module:#?}");
    }
}

fn abort<T>(cli: &Cli, err: impl Error) -> T {
    eprintln!("ERROR: {err}");

    let mut sources = Vec::new();
    let mut current = err.source();
    while let Some(cause) = current {
        sources.push(cause);
        current = cause.source();
    }
    if !sources.is_empty() {
        eprintln!("\nCaused by:");
        for (i, cause) in sources.iter().enumerate() {
            eprintln!("    {i}: {cause}");
        }
    }

    if cli.verbose {
        eprintln!("\nDEBUG OUTPUT:\n{err:#?}");
    }

    exit(1)
}
