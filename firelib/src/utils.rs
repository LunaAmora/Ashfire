use std::{collections::VecDeque, path::Path};

use anyhow;

/// Gets the directory of the [`Path`],
/// or [`None`] if is empty.
pub fn get_dir(current: &Path) -> Option<&Path> {
    current.ancestors().nth(1)
}

/// Maps an empty vector to [`None`], and a non empty to [`Some<Vec<T>>`].
pub fn empty_or_some<T>(vec: Vec<T>) -> Option<Vec<T>> {
    if vec.is_empty() {
        None
    } else {
        Some(vec)
    }
}

/// Consumes and flatten a nested [`Vec`].
pub fn flatten<T>(vec: Vec<Vec<T>>) -> Vec<T> {
    vec.into_iter().flatten().collect()
}

/// Push a value `T` to different [`Vec<T>`] based on the given condition.
pub fn push_by_condition<T>(cond: bool, value: T, if_true: &mut Vec<T>, if_false: &mut Vec<T>) {
    match cond {
        true => if_true.push(value),
        _ => if_false.push(value),
    }
}

/// Gets the index in the [`Vec`] that matches the given predicate.
///
/// # Panics
/// Panics if no matching element is found.
pub fn expect_index<T>(vec: &[T], pred: impl FnMut(&T) -> bool) -> usize {
    vec.iter()
        .position(pred)
        .expect("no item matched the given predicate")
}

pub fn get_range_ref<T, const N: usize>(
    deque: &VecDeque<T>, index: usize,
) -> anyhow::Result<[&T; N]> {
    if deque.len() > (index + N + 1) {
        let range: Vec<&T> = deque.range(index..(index + N)).collect();

        match range.try_into() {
            Ok(t) => Ok(t),
            _ => unreachable!("Failed to collect into an correctly sized array"),
        }
    } else {
        anyhow::bail!(
            "Invalid range of elements to get from the deque: `{}`..`{}` of `{}`",
            index,
            index + N,
            deque.len()
        )
    }
}
