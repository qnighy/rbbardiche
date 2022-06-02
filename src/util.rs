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
