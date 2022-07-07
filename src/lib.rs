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

pub trait OrElse<T> {
    fn or_else(self, f: impl FnOnce() -> T) -> Self;
}

impl<T> OrElse<Result<Option<T>>> for OptionErr<T> {
    fn or_else(mut self, f: impl FnOnce() -> Result<Option<T>>) -> Self {
        if let Ok(None) = self.0 {
            self.0 = f();
        }
        self
    }
}

impl<T> OrElse<Option<T>> for OptionErr<T> {
    fn or_else(mut self, f: impl FnOnce() -> Option<T>) -> Self {
        if let Ok(None) = self.0 {
            self.0 = Ok(f());
        }
        self
    }
}

impl<T> OrElse<T> for OptionErr<T> {
    fn or_else(mut self, f: impl FnOnce() -> T) -> Self {
        if let Ok(None) = self.0 {
            self.0 = Ok(Some(f()));
        }
        self
    }
}

pub trait ExpectBy<T> {
    fn expect_by(self, pred: impl FnOnce(&T) -> bool, desc: &str) -> Result<T>;
}

pub fn get_dir(current: &Path) -> Result<&Path> {
    current.ancestors()
        .nth(1).with_context(|| "failed to get file directory path")
}

#[macro_export]
macro_rules! choice {
    ( $i:expr, $( $x:expr ),* ) => {
        OptionErr::from($i)
        $(
            .or_else(|| $x)
        )*
            .into()
    }
}
