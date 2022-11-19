#![feature(try_trait_v2)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, Deref, FromResidual, Try},
};

use either::Either;
use firelib::{
    alternative,
    anyhow::{Error, Result},
    choice::Alternative,
    lazy::{LazyError, LazyResult},
    FlowControl, Success, SuccessFrom,
};

#[derive(FlowControl)]
#[alternative(value, Ok(None))]
pub struct OptionErr<T, E> {
    pub value: LazyResult<Option<T>, E>,
}

impl<T, E> Alternative for OptionErr<T, E> {
    type ChoiceOutput = Option<T>;
    type ChoiceResidual = LazyResult<Option<T>, E>;
}

impl<T, E> OptionErr<T, E> {
    pub fn new(value: T) -> Self {
        Self { value: Ok(Some(value)) }
    }
}

impl<T, E> Default for OptionErr<T, E> {
    fn default() -> Self {
        Self { value: Ok(None) }
    }
}

impl<T, E> FromResidual<LazyResult<Infallible, E>> for OptionErr<T, E> {
    fn from_residual(residual: LazyResult<Infallible, E>) -> Self {
        match residual {
            Err(lazy) => Self { value: Err(lazy) },
            Ok(_) => unreachable!(),
        }
    }
}

impl<T, E> From<LazyError<E>> for OptionErr<T, E> {
    fn from(value: LazyError<E>) -> Self {
        Self { value: Err(value) }
    }
}

impl<T, E> From<Error> for OptionErr<T, E> {
    fn from(err: Error) -> Self {
        Self { value: Err(LazyError::from(err)) }
    }
}

impl<T, E> From<LazyResult<Option<T>, E>> for OptionErr<T, E> {
    fn from(value: LazyResult<Option<T>, E>) -> Self {
        Self { value }
    }
}

impl<T, E> From<Result<Option<T>>> for OptionErr<T, E> {
    fn from(value: Result<Option<T>>) -> Self {
        match value {
            Ok(ok) => Self::from(ok),
            Err(err) => Self::from(err),
        }
    }
}

impl<T, E> From<Option<T>> for OptionErr<T, E> {
    fn from(value: Option<T>) -> Self {
        Self { value: Ok(value) }
    }
}

impl<T, E> Success for OptionErr<Vec<T>, E> {
    fn success_value() -> Self {
        OptionErr { value: Ok(Some(vec![])) }
    }
}

impl<T, E> SuccessFrom for OptionErr<Vec<T>, E> {
    type From = T;

    fn success_from(from: Self::From) -> Self {
        OptionErr { value: Ok(Some(vec![from])) }
    }
}

pub struct DoubleResult<T, E, F> {
    pub value: Result<T, Either<E, LazyError<F>>>,
}

impl<T, E, F> DoubleResult<T, E, F> {
    pub fn new(value: T) -> Self {
        Self { value: Ok(value) }
    }
}

impl<T, E, F> Deref for DoubleResult<T, E, F> {
    type Target = Result<T, Either<E, LazyError<F>>>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T, E, F> Try for DoubleResult<T, E, F> {
    type Output = T;
    type Residual = Either<E, LazyError<F>>;

    fn from_output(output: Self::Output) -> Self {
        Self { value: Ok(output) }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self.value {
            Ok(o) => ControlFlow::Continue(o),
            Err(r) => ControlFlow::Break(r),
        }
    }
}

impl<T, E, F> FromResidual for DoubleResult<T, E, F> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        Self { value: Err(residual) }
    }
}

impl<T, E, F> FromResidual<Result<Infallible, <Self as Try>::Residual>> for DoubleResult<T, E, F> {
    fn from_residual(residual: Result<Infallible, <Self as Try>::Residual>) -> Self {
        match residual {
            Ok(_) => unreachable!(),
            Err(r) => Self { value: Err(r) },
        }
    }
}

impl<T, E, F> FromResidual<Result<Infallible, LazyError<F>>> for DoubleResult<T, E, F> {
    fn from_residual(residual: Result<Infallible, LazyError<F>>) -> Self {
        Self { value: Err(Either::Right(residual.unwrap_err())) }
    }
}

/// An unchecked Stack API.
pub trait UncheckedStack<T>: Deref<Target = [T]> {
    fn push(&mut self, item: T);
    fn extend<const N: usize>(&mut self, items: [T; N]);
    fn truncate(&mut self, n: usize);
    /// # Safety
    /// Check inner stack lengh before using.
    unsafe fn pop(&mut self) -> T;
    /// # Safety
    /// Check inner stack lengh before using.
    unsafe fn pop_array<const N: usize>(&mut self) -> [T; N];
    /// # Safety
    /// Check inner stack lengh before using.
    unsafe fn peek(&mut self) -> &T;
    /// # Safety
    /// Check inner stack lengh before using.
    unsafe fn get_from_top(&self, n: usize) -> &T;
}

#[derive(Clone)]
pub struct EvalStack<T> {
    frames: Vec<T>,
    pub min_count: i32,
    pub stack_count: i32,
}

impl<T> Deref for EvalStack<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.frames
    }
}

impl<T> Default for EvalStack<T> {
    fn default() -> Self {
        Self { frames: Vec::new(), min_count: 0, stack_count: 0 }
    }
}

impl<T: Clone> EvalStack<T> {
    pub fn new(other: &EvalStack<T>) -> Self {
        Self {
            frames: other.frames.to_vec(),
            min_count: other.min_count,
            stack_count: 0,
        }
    }
}

impl<T> EvalStack<T> {
    pub fn reset_max_count(&mut self) {
        self.stack_count = 0;
    }

    fn stack_minus(&mut self, n: usize) {
        self.stack_count -= n as i32;
        if self.stack_count < self.min_count {
            self.min_count = self.stack_count
        }
    }
}

impl<T> UncheckedStack<T> for EvalStack<T> {
    fn push(&mut self, item: T) {
        self.stack_count += 1;
        self.frames.push(item)
    }

    fn extend<const N: usize>(&mut self, items: [T; N]) {
        self.stack_count += N as i32;
        self.frames.extend(items)
    }

    unsafe fn pop(&mut self) -> T {
        self.stack_minus(1);
        self.frames.pop().unwrap_unchecked()
    }

    fn truncate(&mut self, n: usize) {
        self.stack_minus(n);
        self.frames.truncate(self.len() - n);
    }

    unsafe fn pop_array<const N: usize>(&mut self) -> [T; N] {
        self.stack_minus(N);
        let len = self.len();
        let range = (len - N)..;
        let drain: Vec<T> = self.frames.drain(range).collect();

        drain.try_into().ok().unwrap_unchecked()
    }

    unsafe fn peek(&mut self) -> &T {
        self.stack_minus(1);
        self.stack_count += 1;
        self.frames.get_unchecked(self.len() - 1)
    }

    unsafe fn get_from_top(&self, n: usize) -> &T {
        self.frames.get_unchecked(self.len() - 1 - n)
    }
}
