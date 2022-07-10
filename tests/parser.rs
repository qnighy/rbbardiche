use rbbardiche::pgem_ast::SExp;
use rbbardiche::pos::SourceLocator;
use testfiles::{test_files, InputFile, OutputFile, PendingFile, SnapshotMode};

#[test_files(dir = "tests/parse")]
#[test]
fn test_parser(
    #[suffix = ".rb"] input: InputFile,
    #[suffix = ".pgem.txt"] output_pgem: OutputFile,
    #[suffix = ".errors.txt"] output_errors: OutputFile,
    #[suffix = ".pending.txt"] pending: PendingFile,
) {
    pending.update_pending(|| {
        let source = input.read_bytes();
        let source_locator = SourceLocator::new(&source);
        let (ast, errors) = rbbardiche::parse(&source);
        let pgem_result = format!("{}\n", SExp::from(&ast));

        if output_errors.path.exists() {
            output_pgem.compare(&pgem_result);
        } else {
            output_pgem.compare_with_mode(&pgem_result, SnapshotMode::None);
        }
        let errors_txt = if errors.is_empty() {
            None
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
            Some(errors_txt)
        };
        output_errors.compare_opt(&errors_txt)
    });
}
