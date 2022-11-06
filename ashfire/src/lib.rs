#![feature(try_trait_v2)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, Deref, FromResidual, Try},
};

use either::Either;
use firelib::{
    alternative,
    anyhow::{Error, Result},
    bail,
    choice::Alternative,
    lazy::{LazyError, LazyResult},
    FlowControl, Success, SuccessFrom,
};
use num::FromPrimitive;

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

pub trait Stack<T>: Deref<Target = [T]> {
    fn push(&mut self, item: T);
    fn push_n<const N: usize>(&mut self, items: [T; N]);
    fn pop(&mut self) -> Option<T>;
    fn pop_n<E>(&mut self, n: usize) -> LazyResult<Vec<T>, E>;
    fn pop_array<E, const N: usize>(&mut self) -> LazyResult<[T; N], E>;
    fn peek(&mut self) -> Option<&T>;
    fn get_from(&self, n: usize) -> Option<&T>;
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
            frames: other.frames.clone(),
            min_count: other.min_count,
            stack_count: 0,
        }
    }

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

impl<T: Clone> Stack<T> for EvalStack<T> {
    fn push(&mut self, item: T) {
        self.stack_count += 1;
        self.frames.push(item)
    }

    fn push_n<const N: usize>(&mut self, items: [T; N]) {
        self.stack_count += N as i32;
        self.frames.extend(items)
    }

    fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }

        self.stack_minus(1);
        self.frames.pop()
    }

    fn pop_n<E>(&mut self, n: usize) -> LazyResult<Vec<T>, E> {
        if n == 0 || n > self.len() {
            bail!("Invalid quantity of elements to pop: `{n}`/`{}`", self.len())
        };

        self.stack_minus(n);
        let len = self.len();
        let range = (len - n)..;
        Ok(self.frames.drain(range).collect())
    }

    fn pop_array<E, const N: usize>(&mut self) -> LazyResult<[T; N], E> {
        if N == 0 || N > self.len() {
            bail!("Invalid quantity of elements to pop: `{N}`/`{}`", self.len())
        };

        self.stack_minus(N);
        let len = self.len();
        let range = (len - N)..;
        let drain: Vec<T> = self.frames.drain(range).collect();

        match drain.try_into() {
            Ok(t) => Ok(t),
            _ => unreachable!("Failed to collect into an correctly sized array"),
        }
    }

    fn peek(&mut self) -> Option<&T> {
        if self.is_empty() {
            return None;
        }

        self.stack_minus(1);
        self.stack_count += 1;
        self.frames.last()
    }

    fn get_from(&self, n: usize) -> Option<&T> {
        self.frames.get(self.len() - 1 - n)
    }
}

impl<T> Stack<T> for Vec<T> {
    fn push(&mut self, item: T) {
        self.push(item)
    }

    fn push_n<const N: usize>(&mut self, items: [T; N]) {
        self.extend(items)
    }

    fn pop(&mut self) -> Option<T> {
        self.pop()
    }

    fn pop_n<E>(&mut self, n: usize) -> LazyResult<Vec<T>, E> {
        let range = (self.len() - n)..;
        Ok(self.drain(range).collect())
    }

    fn pop_array<E, const N: usize>(&mut self) -> LazyResult<[T; N], E> {
        todo!()
    }

    fn peek(&mut self) -> Option<&T> {
        self.last()
    }

    fn get_from(&self, n: usize) -> Option<&T> {
        if n > self.len() {
            return None;
        }

        Some(&self[self.len() - 1 - n])
    }
}

#[track_caller]
pub fn from_i32<T: FromPrimitive>(value: i32) -> T {
    FromPrimitive::from_i32(value).unwrap()
}
