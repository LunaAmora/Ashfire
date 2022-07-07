use anyhow::{Result, Context};
use std::path::Path;

pub struct OptionErr<T> (Result<Option<T>>);

impl<T> From<OptionErr<T>> for Result<Option<T>> {
    fn from(val: OptionErr<T>) -> Self {
        val.0
    }
}

impl<T> From<Result<Option<T>>> for OptionErr<T> {
    fn from(opt: Result<Option<T>>) -> Self {
        Self(opt)
    }
}

impl<T> From<Option<T>> for OptionErr<T> {
    fn from(opt: Option<T>) -> Self {
        Self(Ok(opt))
    }
}

impl<T> From<T> for OptionErr<T> {
    fn from(opt: T) -> Self {
        Self(Ok(Some(opt)))
    }
}

impl<T> OptionErr<T> {
    pub fn or_else(mut self, f: impl FnOnce() -> OptionErr<T>) -> Self {
        if let Ok(None) = self.0 {
            self = f();
        }
        self
    }
}

#[macro_export]
macro_rules! choice {
    ( $i:expr, $( $x:expr ),* ) => {
        OptionErr::from($i)
        $(
            .or_else(|| OptionErr::from($x))
        )*
            .into()
    }
}

pub trait ExpectBy<T> {
    fn expect_by(self, pred: impl FnOnce(&T) -> bool, desc: &str) -> Result<T>;
}

pub fn get_dir(current: &Path) -> Result<&Path> {
    current.ancestors()
        .nth(1).with_context(|| "failed to get file directory path")
}
