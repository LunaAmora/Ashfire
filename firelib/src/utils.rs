use std::{collections::VecDeque, iter::Rev, path::Path};

use anyhow::{bail, Result};
use itertools::Either;

/// Gets the directory of the [`Path`],
/// or [`None`] if is empty.
pub fn get_dir(current: &Path) -> Option<&Path> {
    current.ancestors().nth(1)
}

pub trait BoolUtils: Sized {
    /// Push the value to different [`Vecs`][Vec] based on the given condition.
    fn conditional_push(self, condition: bool, if_true: &mut Vec<Self>, if_false: &mut Vec<Self>) {
        if condition {
            if_true.push(self);
        } else {
            if_false.push(self);
        }
    }

    fn conditional_rev(self, condition: bool) -> Either<Rev<Self>, Self>
    where
        Self: Iterator + DoubleEndedIterator,
    {
        if condition {
            Either::Left(self.rev())
        } else {
            Either::Right(self)
        }
    }
}

impl<T> BoolUtils for T {}

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
