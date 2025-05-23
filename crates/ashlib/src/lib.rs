#![feature(try_trait_v2)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, Deref, FromResidual, Try},
};

pub use either::Either;
use firelib::{
    Error, FlowControl, Result, Success, SuccessFrom, alternative, choice::Alternative, lazy,
};

#[derive(FlowControl)]
#[alternative(value, Ok(None))]
pub struct OptionErr<'err, T, E> {
    pub value: lazy::Result<'err, Option<T>, E>,
}

impl<'err, T, E> Alternative for OptionErr<'err, T, E> {
    type ChoiceOutput = Option<T>;
    type ChoiceResidual = lazy::Result<'err, Option<T>, E>;
}

impl<T, E> OptionErr<'_, T, E> {
    pub fn new(value: T) -> Self {
        Self { value: Ok(Some(value)) }
    }
}

impl<T, E> Default for OptionErr<'_, T, E> {
    fn default() -> Self {
        Self { value: Ok(None) }
    }
}

impl<'err, T, E> FromResidual<lazy::Result<'err, Infallible, E>> for OptionErr<'err, T, E> {
    fn from_residual(Err(err): lazy::Result<'err, Infallible, E>) -> Self {
        Self { value: Err(err) }
    }
}

impl<'err, T, E> From<lazy::Error<'err, E>> for OptionErr<'err, T, E> {
    fn from(value: lazy::Error<'err, E>) -> Self {
        Self { value: Err(value) }
    }
}

impl<T, E> From<Error> for OptionErr<'_, T, E> {
    fn from(err: Error) -> Self {
        Self { value: Err(lazy::Error::from(err)) }
    }
}

impl<'err, T, E> From<lazy::Result<'err, Option<T>, E>> for OptionErr<'err, T, E> {
    fn from(value: lazy::Result<'err, Option<T>, E>) -> Self {
        Self { value }
    }
}

impl<'err, T, E> From<lazy::Result<'err, T, E>> for OptionErr<'err, T, E> {
    fn from(res: lazy::Result<'err, T, E>) -> Self {
        match res {
            Ok(value) => Self::new(value),
            Err(err) => Self { value: Err(err) },
        }
    }
}

impl<T, E> From<Option<T>> for OptionErr<'_, T, E> {
    fn from(value: Option<T>) -> Self {
        Self { value: Ok(value) }
    }
}

impl<T, E> Success for OptionErr<'_, Vec<T>, E> {
    fn success_value() -> Self {
        Self { value: Ok(Some(vec![])) }
    }
}

impl<T, E> SuccessFrom for OptionErr<'_, Vec<T>, E> {
    type From = T;

    fn success_from(from: Self::From) -> Self {
        Self { value: Ok(Some(vec![from])) }
    }
}

pub struct DoubleResult<'err, T, E, F> {
    pub value: Result<T, Either<E, lazy::Error<'err, F>>>,
}

impl<T, E, F> DoubleResult<'_, T, E, F> {
    pub fn new(value: T) -> Self {
        Self { value: Ok(value) }
    }
}

impl<'err, T, E, F> Deref for DoubleResult<'err, T, E, F> {
    type Target = Result<T, Either<E, lazy::Error<'err, F>>>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'err, T, E, F> Try for DoubleResult<'err, T, E, F> {
    type Output = T;
    type Residual = Either<E, lazy::Error<'err, F>>;

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

impl<T, E, F> FromResidual for DoubleResult<'_, T, E, F> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        Self { value: Err(residual) }
    }
}

impl<T, E, F> FromResidual<Result<Infallible, <Self as Try>::Residual>>
    for DoubleResult<'_, T, E, F>
{
    fn from_residual(Err(err): Result<Infallible, <Self as Try>::Residual>) -> Self {
        Self { value: Err(err) }
    }
}

impl<'err, T, E, F> FromResidual<Result<Infallible, lazy::Error<'err, F>>>
    for DoubleResult<'err, T, E, F>
{
    fn from_residual(Err(err): Result<Infallible, lazy::Error<'err, F>>) -> Self {
        Self { value: Err(Either::Right(err)) }
    }
}

/// An unchecked Stack API.
///
/// # Panics
///
/// Can panic if the underlying stack length is not checked beforehand.
pub trait UncheckedStack<T>: Deref<Target = [T]> {
    fn push(&mut self, item: T);
    fn extend<const N: usize>(&mut self, items: [T; N]);
    fn truncate(&mut self, n: usize);
    fn pop(&mut self) -> T;
    fn pop_array<const N: usize>(&mut self) -> [T; N];
    fn peek(&mut self) -> &T;
    fn get_from_top(&self, n: usize) -> &T;
}

#[derive(Clone)]
pub struct EvalStack<T> {
    frames: Vec<T>,
    min_count: isize,
    stack_count: isize,
}

impl<T> EvalStack<T> {
    pub fn set_count(&mut self, min: isize, count: isize) {
        self.min_count = min;
        self.stack_count = count;
    }

    pub fn min(&self) -> isize {
        self.min_count
    }

    pub fn count(&self) -> isize {
        self.stack_count
    }

    pub fn into_inner(self) -> Vec<T> {
        self.frames
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
        let i: isize = n.try_into().expect("ICE");
        self.stack_count -= i;
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
        let n = isize::try_from(N).expect("ICE");
        self.stack_count += n;
        self.frames.extend(items);
    }

    fn pop(&mut self) -> T {
        self.stack_minus(1);
        self.frames
            .pop()
            .expect("Could not pop from an empty Stack")
    }

    fn truncate(&mut self, n: usize) {
        self.stack_minus(n);
        self.frames.truncate(self.len() - n);
    }

    fn pop_array<const N: usize>(&mut self) -> [T; N] {
        self.stack_minus(N);
        let len = self.len();
        let range = (len - N)..;
        let drain: Vec<T> = self.frames.drain(range).collect();

        drain.try_into().unwrap_or_else(|_| panic!())
    }

    fn peek(&mut self) -> &T {
        self.stack_minus(1);
        self.stack_count += 1;
        &self.frames[self.len() - 1]
    }

    fn get_from_top(&self, n: usize) -> &T {
        &self.frames[self.len() - 1 - n]
    }
}
