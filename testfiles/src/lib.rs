use bstr::ByteSlice;
use serde::Serialize;
use std::env;
use std::path::{Path, PathBuf};

pub use testfiles_macros::test_files;

macro_rules! impl_conversions {
    ($T:ty) => {
        impl<P: Into<PathBuf>> From<P> for $T {
            fn from(path: P) -> Self {
                Self { path: path.into() }
            }
        }

        impl AsRef<Path> for $T {
            fn as_ref(&self) -> &Path {
                &self.path
            }
        }
    };
}

#[derive(Debug, Clone)]
pub struct InputFile {
    pub path: PathBuf,
}

impl_conversions!(InputFile);

impl InputFile {
    pub fn read_bytes(&self) -> Vec<u8> {
        std::fs::read(self)
            .unwrap_or_else(|e| panic!("Error reading {}: {}", self.path.display(), e))
    }
}

#[derive(Debug, Clone)]
pub struct OptionalInputFile {
    pub path: PathBuf,
}

impl_conversions!(OptionalInputFile);

#[derive(Debug, Clone)]
pub struct OutputFile {
    pub path: PathBuf,
}

impl_conversions!(OutputFile);

impl OutputFile {
    pub fn generic_compare<F>(&self, actual: &[u8], on_diff: F, mode: SnapshotMode)
    where
        F: FnOnce(&[u8]),
    {
        match std::fs::read(self) {
            Ok(expected) => {
                if expected != actual {
                    if mode == SnapshotMode::All {
                        std::fs::write(self, actual).unwrap_or_else(|e| {
                            panic!("Error writing {}: {}", self.path.display(), e)
                        });
                    } else {
                        on_diff(&expected);
                        // Fallback
                        assert_eq!(actual, expected);
                    }
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                if mode >= SnapshotMode::New {
                    std::fs::write(self, actual)
                        .unwrap_or_else(|e| panic!("Error writing {}: {}", self.path.display(), e));
                } else {
                    panic!("Snapshot {} not found\n\nUse UPDATE_SNAPSHOTS=true to generate the snapshot", self.path.display());
                }
            }
            Err(e) => panic!("Error reading {}: {}", self.path.display(), e),
        }
    }

    pub fn compare_string_with_mode(&self, actual: &str, mode: SnapshotMode) {
        self.generic_compare(
            actual.as_bytes(),
            |expected| assert_eq!(actual, expected.as_bstr()),
            mode,
        );
    }

    pub fn compare_string(&self, actual: &str) {
        self.compare_string_with_mode(actual, SnapshotMode::current());
    }

    pub fn compare_json_with_mode<T: Serialize>(&self, actual: &T, mode: SnapshotMode) {
        let actual = serde_json::to_string_pretty(actual).unwrap_or_else(|e| {
            panic!(
                "Error during serialization: {} (in {})",
                e,
                self.path.display()
            )
        });
        self.compare_string_with_mode(&actual, mode);
    }

    pub fn compare_json<T: Serialize>(&self, actual: &T) {
        self.compare_json_with_mode(actual, SnapshotMode::current());
    }

    pub fn remove_with_mode(&self, mode: SnapshotMode) {
        if self.path.exists() {
            if mode == SnapshotMode::All {
                std::fs::remove_file(&self.path).unwrap_or_else(|e| {
                    panic!("Error removing {}: {}", self.path.display(), e);
                });
            } else {
                panic!(
                    "Snapshot {} should not exist\n\nUse UPDATE_SNAPSHOTS=true to remove the unnecessary snapshot",
                    self.path.display()
                );
            }
        }
    }

    pub fn remove(&self) {
        self.remove_with_mode(SnapshotMode::current());
    }
}

#[derive(Debug, Clone)]
pub struct InputDirectory {
    pub path: PathBuf,
}

impl_conversions!(InputDirectory);

#[derive(Debug, Clone)]
pub struct OptionalInputDirectory {
    pub path: PathBuf,
}

impl_conversions!(OptionalInputDirectory);

#[derive(Debug, Clone)]
pub struct OutputDirectory {
    pub path: PathBuf,
}

impl_conversions!(OutputDirectory);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SnapshotMode {
    None,
    New,
    All,
}

impl SnapshotMode {
    pub fn current() -> SnapshotMode {
        let update_snapshots = env::var("UPDATE_SNAPSHOTS").unwrap_or_else(|_| String::from(""));
        let ci = env::var("CI").unwrap_or_else(|_| String::from(""));
        if update_snapshots == "all" || update_snapshots == "true" || update_snapshots == "1" {
            return SnapshotMode::All;
        } else if update_snapshots == "new" {
            return SnapshotMode::New;
        } else if update_snapshots == "none"
            || update_snapshots == "false"
            || update_snapshots == "0"
        {
            return SnapshotMode::None;
        }
        if ci == "true" || ci == "1" {
            return SnapshotMode::None;
        }
        SnapshotMode::New
    }
}
