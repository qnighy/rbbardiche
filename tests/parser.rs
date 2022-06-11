use rbbardiche::{
    ast::{Expr, Range},
    pgem_ast::display_pgem,
    pos::SourceLocator,
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
    #[suffix = ".errors.txt"] output_errors: OutputFile,
) {
    let source = input.read_bytes();
    let source_locator = SourceLocator::new(&source);
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
    if errors.is_empty() {
        output_errors.remove();
    } else {
        let errors_txt = errors
            .iter()
            .map(|e| {
                let start = source_locator.position_utf8(&source, e.range().0);
                let end = source_locator.position_utf8(&source, e.range().1);
                format!(
                    "{}:{}-{}-{}: {}\n",
                    start.line + 1,
                    start.character,
                    end.line + 1,
                    end.character,
                    e
                )
            })
            .collect::<Vec<_>>()
            .join("");
        output_errors.compare_string(&errors_txt);
    }
}
