use std::fs::File;
use std::io::BufReader;
use std::process;

use anyhow::{Context, Result};

fn main() {
    if let Err(err) = run() {
        eprintln!("{err:#}");
        process::exit(1);
    }
}

fn run() -> Result<()> {
    let path = std::env::args().nth(1).context("usage: wadec <FILE>")?;

    let file = File::open(&path).with_context(|| format!("failed to open file `{path}`"))?;
    let reader = BufReader::new(file);

    wadec::decode(reader).context("failed to decode module")?;
    Ok(())
}
