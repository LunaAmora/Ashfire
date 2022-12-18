#![feature(try_trait_v2)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, Deref, FromResidual, Try},
};

pub use either::Either;
use firelib::{
    alternative,
    choice::Alternative,
    lazy::{LazyError, LazyResult},
    Error, FlowControl, Result, Success, SuccessFrom,
};

#[derive(FlowControl)]
#[alternative(value, Ok(None))]
pub struct OptionErr<'err, T, E> {
    pub value: LazyResult<'err, Option<T>, E>,
}

impl<'err, T, E> Alternative for OptionErr<'err, T, E> {
    type ChoiceOutput = Option<T>;
    type ChoiceResidual = LazyResult<'err, Option<T>, E>;
}

impl<'err, T, E> OptionErr<'err, T, E> {
    pub fn new(value: T) -> Self {
        Self { value: Ok(Some(value)) }
    }
}

impl<'err, T, E> Default for OptionErr<'err, T, E> {
    fn default() -> Self {
        Self { value: Ok(None) }
    }
}

impl<'err, T, E> FromResidual<LazyResult<'err, Infallible, E>> for OptionErr<'err, T, E> {
    fn from_residual(residual: LazyResult<'err, Infallible, E>) -> Self {
        match residual {
            Err(lazy) => Self { value: Err(lazy) },
            Ok(_) => unreachable!(),
        }
    }
}

impl<'err, T, E> From<LazyError<'err, E>> for OptionErr<'err, T, E> {
    fn from(value: LazyError<'err, E>) -> Self {
        Self { value: Err(value) }
    }
}

impl<'err, T, E> From<Error> for OptionErr<'err, T, E> {
    fn from(err: Error) -> Self {
        Self { value: Err(LazyError::from(err)) }
    }
}

impl<'err, T, E> From<LazyResult<'err, Option<T>, E>> for OptionErr<'err, T, E> {
    fn from(value: LazyResult<'err, Option<T>, E>) -> Self {
        Self { value }
    }
}

impl<'err, T, E> From<Result<Option<T>>> for OptionErr<'err, T, E> {
    fn from(value: Result<Option<T>>) -> Self {
        match value {
            Ok(ok) => Self::from(ok),
            Err(err) => Self::from(err),
        }
    }
}

impl<'err, T, E> From<Option<T>> for OptionErr<'err, T, E> {
    fn from(value: Option<T>) -> Self {
        Self { value: Ok(value) }
    }
}

impl<'err, T, E> Success for OptionErr<'err, Vec<T>, E> {
    fn success_value() -> Self {
        Self { value: Ok(Some(vec![])) }
    }
}

impl<'err, T, E> SuccessFrom for OptionErr<'err, Vec<T>, E> {
    type From = T;

    fn success_from(from: Self::From) -> Self {
        Self { value: Ok(Some(vec![from])) }
    }
}

pub struct DoubleResult<'err, T, E, F> {
    pub value: Result<T, Either<E, LazyError<'err, F>>>,
}

impl<'err, T, E, F> DoubleResult<'err, T, E, F> {
    pub fn new(value: T) -> Self {
        Self { value: Ok(value) }
    }
}

impl<'err, T, E, F> Deref for DoubleResult<'err, T, E, F> {
    type Target = Result<T, Either<E, LazyError<'err, F>>>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'err, T, E, F> Try for DoubleResult<'err, T, E, F> {
    type Output = T;
    type Residual = Either<E, LazyError<'err, F>>;

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

impl<'err, T, E, F> FromResidual for DoubleResult<'err, T, E, F> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        Self { value: Err(residual) }
    }
}

impl<'err, T, E, F> FromResidual<Result<Infallible, <Self as Try>::Residual>>
    for DoubleResult<'err, T, E, F>
{
    fn from_residual(residual: Result<Infallible, <Self as Try>::Residual>) -> Self {
        match residual {
            Ok(_) => unreachable!(),
            Err(r) => Self { value: Err(r) },
        }
    }
}

impl<'err, T, E, F> FromResidual<Result<Infallible, LazyError<'err, F>>>
    for DoubleResult<'err, T, E, F>
{
    fn from_residual(residual: Result<Infallible, LazyError<'err, F>>) -> Self {
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
    min_count: i32,
    stack_count: i32,
}

impl<T> EvalStack<T> {
    pub fn set_count(&mut self, min: i32, count: i32) {
        self.min_count = min;
        self.stack_count = count;
    }

    pub fn min(&self) -> i32 {
        self.min_count
    }

    pub fn count(&self) -> i32 {
        self.stack_count
    }
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
    pub fn new(other: Self) -> Self {
        Self {
            frames: other.frames,
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
            self.min_count = self.stack_count;
        }
    }
}

impl<T> UncheckedStack<T> for EvalStack<T> {
    fn push(&mut self, item: T) {
        self.stack_count += 1;
        self.frames.push(item);
    }

    fn extend<const N: usize>(&mut self, items: [T; N]) {
        self.stack_count += N as i32;
        self.frames.extend(items);
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

        drain.try_into().unwrap_unchecked()
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