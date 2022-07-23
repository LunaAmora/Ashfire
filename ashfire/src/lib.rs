#![feature(try_trait_v2)]
use anyhow::{Error, Result};

use firelib_macro::Control;
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
    path::Path,
};

pub trait Alternative: Sized {
    fn or_else<F: Into<Self>>(self, f: impl FnOnce() -> F) -> Self;
}

pub trait AlternativeV2: Try<Residual = Self> {}

#[derive(Control)]
pub struct OptionErr<T> {
    pub value: Result<Option<T>>,
}

impl<T> Default for OptionErr<T> {
    fn default() -> Self {
        Ok(None).into()
    }
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

impl<T> Alternative for OptionErr<T> {
    fn or_else<F: Into<Self>>(self, f: impl FnOnce() -> F) -> Self {
        match self.value {
            Ok(None) => f().into(),
            _ => self,
        }
    }
}

impl<T> AlternativeV2 for OptionErr<T> {}

impl<T> Try for OptionErr<T> {
    type Output = Self;
    type Residual = Self;

    fn from_output(output: Self::Output) -> Self {
        output
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self.value {
            Ok(None) => ControlFlow::Continue(self),
            _ => ControlFlow::Break(self),
        }
    }
}

impl<T> FromResidual for OptionErr<T> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        residual
    }
}

impl<T> FromResidual<Result<Infallible, Error>> for OptionErr<T> {
    fn from_residual(residual: Result<Infallible, Error>) -> Self {
        match residual {
            Ok(_) => unreachable!(),
            Err(err) => Self { value: Err(err) },
        }
    }
}

/// Chain `Alternative::or_else` calls in a lazy way,
/// ending the chain with a call to `.into()`.
///
/// # See also
/// The choice function for Alternatives in haskell.
#[macro_export]
macro_rules! choice {
    ( $typ:ident, $i:expr, $( $x:expr ),* ) => {
        $typ::from($i)
        $(
            .or_else(|| $x)
        )*
            .into()
    }
}

#[macro_export]
macro_rules! try_choice {
    ( $typ:ident, $( $x:expr ),*) => {
        {
            let mut __value;
            $(__value = $typ::from($x)?; )*
            __value
        }
    }
}

#[macro_export]
macro_rules! equals_any {
    ($expression:expr, $( $equal:expr ),+) => {
        ($($expression == $equal)||*)
    };
}

#[macro_export]
macro_rules! fold_bool {
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

pub fn map_res<T>(res: Result<()>) -> OptionErr<Vec<T>> {
    res.map(|_| Some(vec![])).into()
}

pub fn map_res_t<T>(res: Result<T>) -> OptionErr<Vec<T>> {
    res.map(|t| Some(vec![t])).into()
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

pub trait FlowControl: Sized {
    fn ensure(condition: bool, f: impl FnOnce() -> Self) -> ControlFlow<Self, ()> {
        match condition {
            true => ControlFlow::Continue(()),
            false => ControlFlow::Break(f()),
        }
    }

    fn from_residual(residual: ControlFlow<Self, Infallible>) -> Self {
        match residual {
            ControlFlow::Break(value) => value,
            _ => unreachable!(),
        }
    }
}

#[macro_export]
macro_rules! ensure {
    ($expr:expr, $( $fmt:expr ),*) => {
        FlowControl::ensure($expr,
            || {
                Err(anyhow::anyhow!( $( $fmt ),* ))?
            }
        )?
    }
}

#[macro_export]
macro_rules! bail {
    ($( $fmt:expr ),*) => {
        Err(anyhow::anyhow!( $( $fmt ),* ))?
    }
}
