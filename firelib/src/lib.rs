#![feature(try_trait_v2)]
#![feature(never_type)]
#![feature(try_blocks)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};

pub use anyhow::{self, bail, Context, Error, Result};
#[cfg(feature = "derive")]
pub use firelib_macro::{alternative, FlowControl};
use lazy::private::Sealed;

pub mod choice;
pub mod lazy;
pub mod lexer;
pub mod utils;

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

/// A trait for giving a type [`bail`] and [`ensure`] macro support.
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
        ControlFlow::Break(<Self as Success>::success_value())
    }

    #[doc(hidden)]
    fn __from_residual(residual: ControlFlow<Self, Infallible>) -> Self {
        match residual {
            ControlFlow::Break(value) => value,
            ControlFlow::Continue(_) => unreachable!(),
        }
    }

    #[doc(hidden)]
    fn __from_error(residual: Result<Infallible, anyhow::Error>) -> Self
    where
        Self: From<anyhow::Error>,
    {
        match residual {
            Err(err) => Self::from(err),
            _ => unreachable!(),
        }
    }
}

pub trait TrySuccess: Sized {
    fn try_success<T, R>(self) -> ControlFlow<T, !>
    where
        Self: Try<Residual = R>,
        T: Success + FromResidual<R>,
    {
        match self.branch() {
            ControlFlow::Continue(_) => ControlFlow::Break(<T as Success>::success_value()),
            ControlFlow::Break(residual) => ControlFlow::Break(T::from_residual(residual)),
        }
    }

    fn into_success<T, R>(self) -> ControlFlow<T, !>
    where
        Self: Try<Residual = R, Output = Self::Internal> + Sealed,
        T: SuccessFrom<From = Self::Internal> + FromResidual<R>,
    {
        match self.branch() {
            ControlFlow::Continue(output) => {
                ControlFlow::Break(<T as SuccessFrom>::success_from(output))
            }
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

/// Checks if the the first element is equivalent (using `==`)
/// to any of the preceding elements.
#[macro_export]
macro_rules! equals_any {
    ($expression:expr, $( $equal:expr ),+) => {
        ($($expression == $equal)||*)
    };
}

/// Ternary-like macro with special support to [`Option`],
/// returning a [`None`] if the condition was false and
/// no false branch was given.
#[macro_export]
macro_rules! fold_bool {
    ($expression:expr, $true:expr) => {
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

/// Creates a new [`Command`][std::process::Command] with the given arguments,
/// prints its content, executes it, then waits for the child process to finish.
#[macro_export]
macro_rules! cmd_wait {
    ($cmd:expr) => {
        info!("[CMD] {}", (format!("{:?}", $cmd).trim_matches('"')));
        std::process::Command::new($cmd).spawn()?.wait()?
    };
    ($cmd:expr, $($arg:expr),*) => {
        let mut __cmd = std::process::Command::new($cmd);
        __cmd$(.arg($arg))*;
        info!("[CMD] {}", (format!("{:?}", __cmd).replace("\"", "")));
        __cmd.spawn()?.wait()?
    };
}

pub trait ShortCircuit<T>: Sealed
where
    T: FromResidual<ControlFlow<T, Infallible>>,
{
    /// Emulates [`ok_or`][Option::ok_or] for returning types that implements
    /// [`FlowControl`] or [`FromResidual<ControlFlow<Self, !>>`].
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
