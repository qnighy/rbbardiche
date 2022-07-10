use std::collections::HashSet;

use walkdir::WalkDir;

#[derive(Debug, Clone)]
pub struct WalkConfig {
    pub rs: String,
    pub dir: String,
    pub arg_specs: Vec<ArgSpec>,
}

#[derive(Debug, Clone)]
pub struct ArgSpec {
    pub suffix: String,
}

pub fn check(config: &WalkConfig, expect_matched_file_names_list: Vec<String>) {
    check2(config, expect_matched_file_names_list).unwrap_or_else(|e| panic!("{}", e));
}

fn check2(
    config: &WalkConfig,
    expect_matched_file_names_list: Vec<String>,
) -> Result<(), std::io::Error> {
    let expect_matched_file_names = expect_matched_file_names_list
        .into_iter()
        .collect::<HashSet<_>>();
    let mut matched_file_names = HashSet::new();
    for entry in WalkDir::new(&config.dir).sort_by_file_name() {
        let entry = entry?;
        let file_name = entry
            .path()
            .strip_prefix(&config.dir)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidInput, e))?;
        let file_name = file_name.to_str().ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::InvalidInput, "Non-UTF8 file name")
        })?;
        for arg_spec in &config.arg_specs {
            if file_name.ends_with(&arg_spec.suffix) {
                matched_file_names.insert(file_name.to_owned());
            }
        }
    }
    if expect_matched_file_names != matched_file_names {
        // Touch the file containing the test
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        utime::set_file_times(&config.rs, now as i64, now as i64)?;

        let mut missing = expect_matched_file_names
            .difference(&matched_file_names)
            .collect::<Vec<_>>();
        missing.sort();
        let mut extra = matched_file_names
            .difference(&expect_matched_file_names)
            .collect::<Vec<_>>();
        extra.sort();

        panic!(
            "Changes detected in testcases. Please rerun the test.\n  missing: {:?}\n  extra: {:?}",
            missing, extra
        )
    }
    Ok(())
}
