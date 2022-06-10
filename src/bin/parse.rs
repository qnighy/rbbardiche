use clap::Parser;
use rbbardiche::parse;
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
            let start = pos(&source, error.range().0);
            let end = pos(&source, error.range().1);
            eprintln!(
                "{}:{}:{}-{}:{}: {}",
                file.display(),
                start.line,
                start.character,
                end.line,
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

#[derive(Debug, Clone, Copy)]
struct Position {
    line: u32,
    character: u32,
}

// TODO: switch to more efficient implementation
fn pos(s: &str, at: usize) -> Position {
    let mut pos = Position {
        line: 0,
        character: 0,
    };
    let at = {
        let mut at = at;
        while !s.is_char_boundary(at) && at < s.len() {
            at += 1;
        }
        at
    };
    for ch in s[..at].chars() {
        if ch == '\n' {
            pos.line += 1;
            pos.character = 0;
        } else if (ch as u32) < 0x10000 {
            pos.character += 1;
        } else {
            pos.character += 2;
        }
    }

    pos
}

#[derive(Debug, Parser)]
struct Cli {
    files: Vec<PathBuf>,
}
