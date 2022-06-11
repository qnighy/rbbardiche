use clap::Parser;
use rbbardiche::parse;
use rbbardiche::pos::SourceLocator;
use std::panic::catch_unwind;
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
        let source_locator = SourceLocator::new(source.as_bytes());
        let result = catch_unwind(|| parse(source.as_bytes()));
        let result = match result {
            Ok(result) => result,
            Err(e) => {
                let msg = if let Some(&e) = e.downcast_ref::<&'static str>() {
                    e
                } else if let Some(e) = e.downcast_ref::<String>() {
                    &e[..]
                } else {
                    "Box<Any>"
                };
                has_error = true;
                eprintln!("{}: {}", file.display(), msg);
                continue;
            }
        };
        let (_, errors) = result;
        for error in &errors {
            has_error = true;
            let start = source_locator.position_utf8(source.as_bytes(), error.range().0);
            let end = source_locator.position_utf8(source.as_bytes(), error.range().1);
            eprintln!(
                "{}:{}:{}-{}:{}: {}",
                file.display(),
                start.line + 1,
                start.character,
                end.line + 1,
                end.character,
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
