use clap::Parser;
use rbbardiche::parse;
use std::path::PathBuf;
use thiserror::Error;

#[derive(Debug, Error)]
enum CommandError {
    #[error("I/O error")]
    Io(
        #[from]
        #[source]
        std::io::Error,
    ),
    #[error("Detected one or more errors")]
    HasError,
}

fn main() -> Result<(), CommandError> {
    let cli = Cli::parse();
    let mut has_error = false;
    for file in &cli.files {
        let source = std::fs::read_to_string(file)?;
        let (_, errors) = parse(source.as_bytes());
        for error in &errors {
            has_error = true;
            eprintln!(
                "{}:{}:{}: {}",
                file.display(),
                error.range().0,
                error.range().1,
                error
            );
        }
    }
    if has_error {
        return Err(CommandError::HasError);
    }
    Ok(())
}

#[derive(Debug, Parser)]
struct Cli {
    files: Vec<PathBuf>,
}
