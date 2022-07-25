#![feature(try_trait_v2)]
use anyhow::{Error, Result};
use firelib::{Alternative, FlowControl, Success, SucessFrom};
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};

#[derive(FlowControl)]
pub struct OptionErr<T> {
    pub value: Result<Option<T>>,
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

impl<T> From<T> for OptionErr<T> {
    fn from(value: T) -> Self {
        Self { value: Ok(Some(value)) }
    }
}

impl<T> Alternative for OptionErr<T> {}

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
