use rbbardiche::ast::{Expr, Range};
use serde::Serialize;
use testfiles::{test_files, InputFile, OutputFile};

#[derive(Debug, Clone, Serialize)]
struct ParseResult {
    ast: Expr,
    errors: Vec<SerializedParseError>,
}

#[derive(Debug, Clone, Serialize)]
struct SerializedParseError {
    message: String,
    range: Range,
}

#[test_files(dir = "tests/parse")]
#[test]
fn test_parser(#[suffix = ".rb"] input: InputFile, #[suffix = ".json"] output: OutputFile) {
    let source = input.read_bytes();
    let (ast, errors) = rbbardiche::parse(&source);
    let result = ParseResult {
        ast,
        errors: errors
            .iter()
            .map(|e| SerializedParseError {
                message: e.to_string(),
                range: e.range(),
            })
            .collect::<Vec<_>>(),
    };
    output.compare_json(&result);
}
