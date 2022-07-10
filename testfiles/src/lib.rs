use bstr::ByteSlice;
use std::env;
use std::panic::{catch_unwind, resume_unwind, AssertUnwindSafe};
use std::path::{Path, PathBuf};

pub use testfiles_macros::test_files;

#[path = "rt.rs"]
pub mod __rt;

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
    pub fn compare_with_mode<T: Snapshot>(&self, actual: &T, mode: SnapshotMode) {
        if let Some(expected) = self.read_bytes_opt() {
            if !actual.compare_with(&expected) {
                if mode == SnapshotMode::All {
                    std::fs::write(self, actual.to_snapshot())
                        .unwrap_or_else(|e| panic!("Error writing {}: {}", self.path.display(), e));
                } else {
                    actual.on_diff(&expected);
                }
            }
        } else {
            if mode >= SnapshotMode::New {
                std::fs::write(self, actual.to_snapshot())
                    .unwrap_or_else(|e| panic!("Error writing {}: {}", self.path.display(), e));
            } else {
                panic!(
                    "Snapshot {} not found\n\nUse UPDATE_SNAPSHOTS=true to generate the snapshot",
                    self.path.display()
                );
            }
        }
    }

    pub fn compare<T: Snapshot>(&self, actual: &T) {
        self.compare_with_mode(actual, SnapshotMode::current());
    }

    pub fn compare_opt_with_mode<T: Snapshot>(&self, actual: &Option<T>, mode: SnapshotMode) {
        let expected = self.read_bytes_opt();
        let comparison = match (actual, &expected) {
            (Some(actual), Some(expected)) => actual.compare_with(expected),
            (None, None) => true,
            _ => false,
        };
        if !comparison {
            if mode == SnapshotMode::All {
                if let Some(actual) = actual {
                    std::fs::write(self, actual.to_snapshot())
                        .unwrap_or_else(|e| panic!("Error writing {}: {}", self.path.display(), e));
                } else {
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
            } else {
                match (actual, &expected) {
                    (Some(actual), Some(expected)) => actual.on_diff(expected),
                    (None, None) => unreachable!(),
                    (Some(_), None) => panic!("Expected None, got Some"),
                    (None, Some(_)) => panic!("Expected Some, got None"),
                };
            }
        }
    }

    pub fn compare_opt<T: Snapshot>(&self, actual: &Option<T>) {
        self.compare_opt_with_mode(actual, SnapshotMode::current());
    }

    fn read_bytes_opt(&self) -> Option<Vec<u8>> {
        match std::fs::read(self) {
            Ok(expected) => Some(expected),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
            Err(e) => panic!("Error reading {}: {}", self.path.display(), e),
        }
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

#[derive(Debug, Clone)]
pub struct PendingFile {
    pub path: PathBuf,
}

impl_conversions!(PendingFile);

impl PendingFile {
    pub fn update_pending_with<R, F>(&self, mode: PendingMode, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let result = catch_unwind(AssertUnwindSafe(f));
        match result {
            Ok(result) => {
                if mode == PendingMode::Update {
                    std::fs::remove_file(&self.path).unwrap_or_else(|e| {
                        if e.kind() != std::io::ErrorKind::NotFound {
                            panic!("Error removing {}: {}", self.path.display(), e);
                        }
                    });
                }
                result
            }
            Err(e) => {
                if mode == PendingMode::Update {
                    let message = if let Some(&e) = e.downcast_ref::<&'static str>() {
                        e
                    } else if let Some(e) = e.downcast_ref::<String>() {
                        &e[..]
                    } else {
                        "Box<dyn Any>"
                    };
                    std::fs::write(&self.path, message).unwrap_or_else(|e| {
                        panic!("Error writing {}: {}", self.path.display(), e);
                    });
                }
                resume_unwind(e);
            }
        }
    }
    pub fn update_pending<R, F>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        self.update_pending_with(PendingMode::current(), f)
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PendingMode {
    Keep,
    Update,
}

impl PendingMode {
    pub fn current() -> PendingMode {
        let update_pending = env::var("UPDATE_PENDING").unwrap_or_else(|_| String::from(""));
        if update_pending == "true" || update_pending == "1" {
            PendingMode::Update
        } else {
            PendingMode::Keep
        }
    }
}

pub trait Snapshot {
    fn to_snapshot(&self) -> Vec<u8>;
    fn compare_with(&self, snapshot: &[u8]) -> bool;
    fn on_diff(&self, snapshot: &[u8]) -> !;
}

impl Snapshot for Vec<u8> {
    fn to_snapshot(&self) -> Vec<u8> {
        self.clone()
    }
    fn compare_with(&self, snapshot: &[u8]) -> bool {
        self == snapshot
    }
    fn on_diff(&self, snapshot: &[u8]) -> ! {
        assert_eq!(self, snapshot);
        unreachable!();
    }
}

impl Snapshot for String {
    fn to_snapshot(&self) -> Vec<u8> {
        self.as_bytes().to_owned()
    }
    fn compare_with(&self, snapshot: &[u8]) -> bool {
        self.as_bytes() == snapshot
    }
    fn on_diff(&self, snapshot: &[u8]) -> ! {
        assert_eq!(self, snapshot.as_bstr());
        unreachable!();
    }
}
