#![feature(try_trait_v2)]
#![feature(never_type)]
#![feature(try_blocks)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
    process::{Child, ChildStdin, ChildStdout, Command, Output},
};

pub use anyhow::{self, bail, Context, Error, Result};
#[cfg(feature = "derive")]
pub use firelib_macro::{alternative, FlowControl};
use lazy::private::{Sealed, SealedT};
use log::info;

pub mod choice;
pub mod lazy;
pub mod lexer;
pub mod span;
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
        Self::from(residual.unwrap_err())
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

    fn into_success<'err, T, R, F>(self) -> ControlFlow<T, !>
    where
        Self: Try<Residual = R, Output = Self::Internal> + SealedT<'err, F>,
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

pub fn command_info(cmd: &Command) -> String {
    format!("{cmd:?}").replace('\"', "")
}

/// Creates a new [`Command`][Command] with the given arguments,
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
        info!("[CMD] {}", $crate::command_info(&__cmd));
        __cmd.spawn()?.wait()?
    };
}

pub struct ChildGuard {
    guard: Option<Child>,
    info: String,
}

impl ChildGuard {
    pub fn new(child: Child, info: String) -> Self {
        Self { guard: Some(child), info }
    }

    pub fn take(mut self) -> Child {
        self.guard.take().expect("Failed to take the `Child`")
    }

    pub fn stdin(&mut self) -> Option<ChildStdin> {
        match &mut self.guard {
            Some(child) => child.stdin.take(),
            None => None,
        }
    }

    pub fn stdout(&mut self) -> Option<ChildStdout> {
        match &mut self.guard {
            Some(child) => child.stdout.take(),
            None => None,
        }
    }

    pub fn wait_with_output(self) -> std::io::Result<Output> {
        info!("[CMD] | {}", self.info);
        self.take().wait_with_output()
    }

    pub fn wait_with_result(self) -> Result<()> {
        let out = self.wait_with_output()?;

        if out.status.success() {
            Ok(())
        } else {
            bail!("{}", String::from_utf8_lossy(&out.stderr))
        }
    }
}

impl Drop for ChildGuard {
    fn drop(&mut self) {
        if let Some(mut child) = self.guard.take() {
            if matches!(child.try_wait(), Ok(None)) {
                if let Err(e) = child.kill() {
                    eprintln!("Could not kill child process: {e}");
                }
            }
        };
    }
}

/// Creates a new [`Command`][std::process::Command] with the given arguments
/// and configured pipes, spawns it and returns inside an `kill on drop` struct.
#[macro_export]
macro_rules! cmd_piped {
    ($cmd:expr, $($arg:expr),* ) => {{
        let mut cmd = std::process::Command::new($cmd);
        cmd
            $(.arg($arg))*
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        let info = $crate::command_info(&cmd);
        $crate::ChildGuard::new(cmd.spawn()?, info)
    }};

    ($cmd:expr, $($arg:expr),* => $stdout:expr) => {{
        let mut cmd = std::process::Command::new($cmd);
        cmd
            $(.arg($arg))*
            .stdin(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .stdout($stdout);

        let info = $crate::command_info(&cmd);
        $crate::ChildGuard::new(cmd.spawn()?, info)
    }};

    ($stdin:expr => $cmd:expr, $($arg:expr),* ) => {{
        let mut cmd = std::process::Command::new($cmd);
        cmd
            $(.arg($arg))*
            .stderr(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stdin($stdin);

        let info = $crate::command_info(&cmd);
        $crate::ChildGuard::new(cmd.spawn()?, info)
    }};
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
