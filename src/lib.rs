use anyhow::Result;
use std::{path::Path, ops::Deref};

pub struct OptionErr<T> {
    value: Result<Option<T>>
}

impl<T> Deref for OptionErr<T> {
    type Target = Result<Option<T>>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> From<OptionErr<T>> for Result<Option<T>> {
    fn from(val: OptionErr<T>) -> Self {
        val.value
    }
}

impl<T> From<Result<Option<T>>> for OptionErr<T> {
    fn from(value: Result<Option<T>>) -> Self {
        Self {value}
    }
}

impl<T> From<Option<T>> for OptionErr<T> {
    fn from(value: Option<T>) -> Self {
        Self {value: Ok(value)}
    }
}

impl<T> From<T> for OptionErr<T> {
    fn from(value: T) -> Self {
        Self {value: Ok(Some(value))}
    }
}

impl<T> OptionErr<T> {
    pub fn or_else(mut self, f: impl FnOnce() -> OptionErr<T>) -> Self {
        if let Ok(None) = *self {
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

pub fn get_dir(current: &Path) -> Option<&Path> {
    current.ancestors().nth(1)
}

pub fn empty_or_some<T>(vec: Vec<T>) -> Option<Vec<T>> {
    if vec.is_empty() {
        None
    } else {
        Some(vec)
    }
}

pub fn flatten<T>(vec: Vec<Vec<T>>) -> Vec<T> {
    vec.into_iter().flatten().collect()
}

pub fn map_res<T>(res: Result<()>) -> Result<Option<Vec<T>>> {
    res.map(|_| Some(vec![]))
}

pub fn map_res_t<T>(res: Result<T>) -> Result<Option<Vec<T>>> {
    res.map(|t| Some(vec![t]))
}

pub fn push_by_condition<T>(cond: bool, value: T, if_true: &mut Vec<T>, if_false: &mut Vec<T>) {
    match cond {
        true => if_true.push(value),
        _   => if_false.push(value),
    }
}
