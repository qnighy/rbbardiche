use testfiles::{test_files, InputFile, OutputFile};

#[test_files(dir = "tests/parse")]
#[test]
fn test_parser(#[suffix = ".rb"] input: InputFile, #[suffix = ".json"] output: OutputFile) {
    let source = input.read_bytes();
    let node = rbbardiche::parse(&source);
    output.compare_json(&node);
}
