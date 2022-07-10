use rbbardiche::pgem_ast::SExp;
use rbbardiche::pos::SourceLocator;
use testfiles::{test_files, InputFile, OutputFile, PendingFile};

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
    });
}
