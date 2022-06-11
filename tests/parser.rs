use rbbardiche::{
    ast::{Expr, Range},
    pgem_ast::display_pgem,
};
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
fn test_parser(
    #[suffix = ".rb"] input: InputFile,
    #[suffix = ".json"] output: OutputFile,
    #[suffix = ".pgem.txt"] output_pgem: OutputFile,
) {
    let source = input.read_bytes();
    let (ast, errors) = rbbardiche::parse(&source);
    let pgem_result = format!("{}\n", display_pgem(&ast));
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
    output_pgem.compare_string(&pgem_result);
}
