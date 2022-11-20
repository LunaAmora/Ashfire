use std::{collections::VecDeque, path::Path};

use anyhow::{bail, Result};

/// Gets the directory of the [`Path`],
/// or [`None`] if is empty.
pub fn get_dir(current: &Path) -> Option<&Path> {
    current.ancestors().nth(1)
}

pub trait EmptySome: Sized {
    fn empty_or_some(self) -> Option<Self>;
}

impl<T> EmptySome for Vec<T> {
    /// Maps an empty vector to [`None`], and a non empty to [`Some<Vec<T>>`].
    fn empty_or_some(self) -> Option<Self> {
        if self.is_empty() {
            None
        } else {
            Some(self)
        }
    }
}

/// Push a value `T` to different [`Vec<T>`] based on the given condition.
pub fn push_by_condition<T>(cond: bool, value: T, if_true: &mut Vec<T>, if_false: &mut Vec<T>) {
    if cond {
        if_true.push(value);
    } else {
        if_false.push(value);
    }
}

/// Gets the index in the [`Vec`] that matches the given predicate.
///
/// # Panics
/// Panics if no matching element is found.
#[track_caller]
pub fn expect_index<T>(vec: &[T], pred: impl FnMut(&T) -> bool) -> usize {
    vec.iter()
        .position(pred)
        .expect("no item matched the given predicate")
}

pub trait RangeRef<T> {
    fn get_range_ref<const N: usize>(&self, index: usize) -> Result<[&T; N]>;
}

impl<T> RangeRef<T> for VecDeque<T> {
    fn get_range_ref<const N: usize>(&self, index: usize) -> Result<[&T; N]> {
        let end_range = index + N;
        let len = self.len();
        let range = index..end_range;

        if len > (end_range + 1) {
            let range: Vec<&T> = self.range(range).collect();
            return Ok(range.try_into().ok().unwrap());
        }

        bail!("Range of elements out of bounds: `{range:?}` from length `{len}`")
    }
}

pub fn strip_trailing_newline(input: &str) -> &str {
    input
        .strip_suffix("\r\n")
        .or_else(|| input.strip_suffix('\n'))
        .unwrap_or(input)
}
