use anyhow::Result;
use std::path::Path;

pub struct OptionErr<T> {
    value: Result<Option<T>>,
}

impl<T> From<OptionErr<T>> for Result<Option<T>> {
    fn from(val: OptionErr<T>) -> Self {
        val.value
    }
}

impl<T> From<Result<Option<T>>> for OptionErr<T> {
    fn from(value: Result<Option<T>>) -> Self {
        Self { value }
    }
}

impl<T> From<Option<T>> for OptionErr<T> {
    fn from(value: Option<T>) -> Self {
        Self { value: Ok(value) }
    }
}

impl<T> From<T> for OptionErr<T> {
    fn from(value: T) -> Self {
        Self { value: Ok(Some(value)) }
    }
}

impl<T> OptionErr<T> {
    pub fn or_else(mut self, f: impl FnOnce() -> Self) -> Self {
        if let Ok(None) = self.value {
            self = f();
        }
        self
    }
}

/// Chain `.or_else(mut self, f: impl Fn() -> Self) -> Self` calls.
///
/// This attemps to convert every argument expression
/// to the given type with `type::from(expr)` in a lazy way.
/// Ending the chain with a call to `.into()`.
///
/// # See also
/// The choice function for Alternatives in haskell.
#[macro_export]
macro_rules! choice {
    ( $typ:ident, $i:expr, $( $x:expr ),* ) => {
        $typ::from($i)
        $(
            .or_else(|| $typ::from($x))
        )*
            .into()
    }
}

#[macro_export]
macro_rules! equals_any {
    ($expression:expr, $( $equal:expr ),+) => {
        ($($expression == $equal)||*)
    };
}

#[macro_export]
macro_rules! map_bool {
    ($expression:expr, $true:expr ) => {
        if $expression {
            $true
        } else {
            None
        }
    };
    ($expression:expr, $true:expr, $false:expr) => {
        if $expression {
            $true
        } else {
            $false
        }
    };
}

pub trait ExpectBy<T> {
    fn expect_by(
        self, pred: impl FnOnce(&T) -> bool, desc: &str, fmt: impl FnOnce(T) -> String,
    ) -> Result<T>;
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
        _ => if_false.push(value),
    }
}

pub fn expect_index<T>(vec: &[T], pred: impl FnMut(&T) -> bool) -> usize {
    vec.iter()
        .position(pred)
        .expect("no item matched the given predicate")
}

pub fn expect_get<T>(vec: &[T], index: usize) -> &T {
    vec.get(index).expect("index out of bounds")
}
