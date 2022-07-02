pub(crate) trait OptionPredExt<T> {
    fn is_some_and_<F>(&self, pred: F) -> bool
    where
        F: FnOnce(&T) -> bool;

    fn is_none_or_<F>(&self, pred: F) -> bool
    where
        F: FnOnce(&T) -> bool;
}

impl<T> OptionPredExt<T> for Option<T> {
    fn is_some_and_<F>(&self, pred: F) -> bool
    where
        F: FnOnce(&T) -> bool,
    {
        if let Some(x) = self {
            pred(x)
        } else {
            false
        }
    }

    fn is_none_or_<F>(&self, pred: F) -> bool
    where
        F: FnOnce(&T) -> bool,
    {
        if let Some(x) = self {
            pred(x)
        } else {
            true
        }
    }
}

pub(crate) trait CharExt {
    /// Equivalent to isspace(3)
    fn isspace(self) -> bool;
}

impl CharExt for u8 {
    fn isspace(self) -> bool {
        self.is_ascii_whitespace() || self == b'\x0B'
    }
}

impl CharExt for char {
    fn isspace(self) -> bool {
        self.is_ascii_whitespace() || self == '\x0B'
    }
}
