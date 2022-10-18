#![feature(try_trait_v2)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, Deref, FromResidual, Try},
};

use anyhow::{bail, Error, Result};
use either::Either;
use firelib::{alternative, FlowControl, Success, SuccessFrom};

#[derive(FlowControl)]
#[alternative(value, Ok(None))]
pub struct OptionErr<T> {
    pub value: Result<Option<T>>,
}

impl<T> OptionErr<T> {
    pub fn new(value: T) -> Self {
        Self { value: Ok(Some(value)) }
    }
}

impl<T> Default for OptionErr<T> {
    fn default() -> Self {
        Ok(None).into()
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

impl<T> From<Error> for OptionErr<T> {
    fn from(err: Error) -> Self {
        Self { value: Err(err) }
    }
}

impl<T> Success for OptionErr<Vec<T>> {
    fn success_value() -> Self {
        OptionErr::from(Ok(Some(vec![])))
    }
}

impl<T> SuccessFrom for OptionErr<Vec<T>> {
    type From = T;

    fn success_from(from: Self::From) -> Self {
        OptionErr::from(Ok(Some(vec![from])))
    }
}

#[derive(FlowControl)]
pub struct DoubleResult<T, E> {
    pub value: Result<T, Either<E, Error>>,
}

impl<T, E> DoubleResult<T, E> {
    pub fn new(value: T) -> Self {
        Self { value: Ok(value) }
    }
}

impl<T, E> From<Error> for DoubleResult<T, E> {
    fn from(err: Error) -> Self {
        Self { value: Err(Either::Right(err)) }
    }
}

impl<T, E> Deref for DoubleResult<T, E> {
    type Target = Result<T, Either<E, Error>>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T, E> Try for DoubleResult<T, E> {
    type Output = T;
    type Residual = Either<E, Error>;

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

impl<T, E> FromResidual for DoubleResult<T, E> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        Self { value: Err(residual) }
    }
}

impl<T, E> FromResidual<Result<Infallible, <Self as Try>::Residual>> for DoubleResult<T, E> {
    fn from_residual(residual: Result<Infallible, <Self as Try>::Residual>) -> Self {
        match residual {
            Ok(_) => unreachable!(),
            Err(r) => Self { value: Err(r) },
        }
    }
}

pub trait Stack<T>: Deref<Target = [T]> {
    fn push(&mut self, item: T);
    fn push_n<const N: usize>(&mut self, items: [T; N]);
    fn pop(&mut self) -> Option<T>;
    fn pop_n(&mut self, n: usize) -> Result<Vec<T>>;
    fn pop_array<const N: usize>(&mut self) -> Result<[T; N]>;
    fn peek(&mut self) -> Option<&T>;
    fn get(&self, n: usize) -> Option<&T>;
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

    fn pop_n(&mut self, n: usize) -> Result<Vec<T>> {
        if n == 0 || n > self.len() {
            bail!("Invalid quantity of elements to pop: `{n}`/`{}`", self.len())
        };

        self.stack_minus(n);
        let len = self.len();
        let range = (len - n)..;
        Ok(self.frames.drain(range).collect())
    }

    fn pop_array<const N: usize>(&mut self) -> Result<[T; N]> {
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

    fn get(&self, n: usize) -> Option<&T> {
        self.frames.get(self.len() - 1 - n)
    }
}
