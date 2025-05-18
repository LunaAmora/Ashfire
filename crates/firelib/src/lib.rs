#![feature(try_trait_v2)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(trait_alias)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};

pub use anyhow::{self, Context, Error, Result};
#[cfg(feature = "derive")]
pub use firelib_macro::{FlowControl, alternative};
use lazy::{Error as LazyError, private::Sealed};

pub mod choice;
pub mod cmd;
pub mod lazy;
pub mod lexer;
pub mod span;
pub mod utils;

/// A trait for giving a type some [`ControlFlow`] related helper functions.
pub trait FlowControl:
    Sized
    + FromResidual<ControlFlow<Self, Infallible>>
    + FromResidual<Result<Infallible, anyhow::Error>>
{
    fn ensure(condition: bool, f: impl FnOnce() -> Self) -> ControlFlow<Self, ()> {
        if condition {
            ControlFlow::Continue(())
        } else {
            ControlFlow::Break(f())
        }
    }

    fn success() -> ControlFlow<Self, !>
    where
        Self: Success,
    {
        ControlFlow::Break(Self::success_value())
    }

    #[doc(hidden)]
    fn __from_residual(residual: ControlFlow<Self, Infallible>) -> Self {
        let ControlFlow::Break(value) = residual;
        value
    }

    #[doc(hidden)]
    fn __from_error(Err(err): Result<Infallible, Error>) -> Self
    where
        Self: From<Error>,
    {
        Self::from(err)
    }
}

pub trait ShortCircuit<T>: Sealed
where
    T: FromResidual<ControlFlow<T, Infallible>>,
{
    /// Emulates [`ok_or`][Option::ok_or] for returning types that implements
    /// [`FromResidual<ControlFlow<Self, !>>`].
    ///
    /// Can be used on [`Result`], [`Option`] and [`bool`].
    fn or_return(self, f: impl FnOnce() -> T) -> ControlFlow<T, Self::Internal>;
}

impl<A: Sealed, T> ShortCircuit<T> for A
where
    T: FromResidual<ControlFlow<T, Infallible>>,
{
    fn or_return(self, f: impl FnOnce() -> T) -> ControlFlow<T, Self::Internal> {
        self.value()
            .map_or_else(|_| ControlFlow::Break(f()), |v| ControlFlow::Continue(v))
    }
}

/// Emulates [`anyhow::ensure`] for returning types that implements [`FlowControl`].
#[macro_export]
macro_rules! ensure {
    ($expr:expr, $( $fmt:expr ),*) => {
        $crate::FlowControl::ensure($expr, || Err($crate::anyhow::anyhow!( $( $fmt ),* ))?)?
    };
}

/// Emulates [`anyhow::bail`] for returning types that implements [`FlowControl`].
#[macro_export]
macro_rules! bail {
    ($( $fmt:expr ),*) => {
        return Err($crate::anyhow::anyhow!( $( $fmt ),* ))?;
    };
}

/// Provides the [`success`][Success::success_value] method
/// that returns a `default-like` success value.
///
/// A trait for giving a type [`success`] macro and [`try_success`][TrySuccess::try_success] support.
pub trait Success {
    fn success_value() -> Self;
}

/// Provides the [`success_from`][SuccessFrom::success_from] method
/// that pass a value to a `From-like` constructor.
///
/// A trait for giving a type [`into_success`][TrySuccess::into_success] support.
pub trait SuccessFrom {
    type From;
    fn success_from(from: Self::From) -> Self;
}

pub trait TrySuccess: Sized {
    fn try_success<T, R>(self) -> ControlFlow<T, !>
    where
        Self: Try<Residual = R>,
        T: Success + FromResidual<R>,
    {
        match self.branch() {
            ControlFlow::Continue(_) => ControlFlow::Break(T::success_value()),
            ControlFlow::Break(residual) => ControlFlow::Break(T::from_residual(residual)),
        }
    }

    fn into_success<'err, T, R, F>(self) -> ControlFlow<T, !>
    where
        Self: Try<Residual = R, Output = Self::Internal> + Sealed<Error = LazyError<'err, F>>,
        T: SuccessFrom<From = Self::Internal> + FromResidual<R>,
    {
        match self.branch() {
            ControlFlow::Continue(output) => ControlFlow::Break(T::success_from(output)),
            ControlFlow::Break(residual) => ControlFlow::Break(T::from_residual(residual)),
        }
    }
}

impl<T> TrySuccess for T {}

/// Returns early with the [`Success`][Success::success_value] impl of the returning type.
#[macro_export]
macro_rules! success {
    () => {
        $crate::FlowControl::success()?;
    };
}
