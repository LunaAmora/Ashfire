#![feature(try_trait_v2)]
use anyhow::{Error, Result};
use firelib::{alternative, FlowControl, Success, SucessFrom};
use std::{
    convert::Infallible,
    ops::{ControlFlow, Deref, FromResidual, Try},
};

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
    fn success() -> Self {
        OptionErr::from(Ok(Some(vec![])))
    }
}

impl<T> SucessFrom for OptionErr<Vec<T>> {
    type From = T;

    fn success_from(from: Self::From) -> Self {
        OptionErr::from(Ok(Some(vec![from])))
    }
}

pub trait Stack<T> {
    fn push(&mut self, item: T);
    fn pop(&mut self) -> Option<T>;
    fn pop_n(&mut self, n: usize) -> Vec<T>;
    fn peek(&mut self) -> Option<&T>;
    fn get(&self, n: usize) -> Option<&T>;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
}

pub struct EvalStack<T> {
    frames: Vec<T>,
    min_count: i32,
    stack_count: i32,
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

impl<T: Clone> Clone for EvalStack<T> {
    fn clone(&self) -> Self {
        Self {
            frames: self.frames.clone(),
            min_count: self.min_count,
            stack_count: 0,
        }
    }
}

impl<T> EvalStack<T> {
    pub fn new() -> Self {
        Self { ..Default::default() }
    }

    fn stack_minus(&mut self, n: usize) {
        self.stack_count -= n as i32;
        if self.stack_count < self.min_count {
            self.min_count = self.stack_count
        }
    }
}

impl<T> Stack<T> for EvalStack<T> {
    fn push(&mut self, item: T) {
        self.stack_count += 1;
        self.frames.push(item)
    }

    fn pop(&mut self) -> Option<T> {
        self.stack_minus(1);
        self.frames.pop()
    }

    fn pop_n(&mut self, n: usize) -> Vec<T> {
        self.stack_minus(n);
        let len = self.len();
        let range = (len - n)..;
        self.frames.drain(range).collect()
    }

    fn peek(&mut self) -> Option<&T> {
        self.stack_minus(1);
        self.stack_count += 1;
        self.frames.last()
    }

    fn get(&self, n: usize) -> Option<&T> {
        self.frames.get(self.len() - 1 - n)
    }

    fn len(&self) -> usize {
        self.frames.len()
    }

    fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }
}
