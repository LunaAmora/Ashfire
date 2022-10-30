#![feature(try_trait_v2)]
#![feature(never_type)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};

pub use anyhow::{self, bail as anybail, ensure as any_ensure};
#[cfg(feature = "derive")]
pub use firelib_macro::{alternative, FlowControl};

pub mod lazy;
pub mod lexer;
pub mod utils;

/// A trait for giving a type [`choice`] macro support.
pub trait Alternative: Try<Residual = Self> {}

#[doc(hidden)]
pub trait __Alternative: Alternative {
    fn __from<U>(it: U) -> Self
    where
        Self: From<U>,
    {
        impl<T: Alternative> __Alternative for T {}
        Self::from(it)
    }
}

/// Chain lazily evaluated [`Alternative::from`][Alternative] calls,
/// returning early when a valid value for the given
/// type is found.
///
/// # See also
///
/// The choice function for Alternatives in haskell.
///
/// # Examples
///
/// ```
/// # #![feature(try_trait_v2)]
/// # use firelib::{choice, alternative};
/// // Here we create an struct that implements the Alternative
/// // trait with a neutral matching pattern of `value: None`.
/// #[alternative(value, None)]
/// struct Alter<T> {
///     pub value: Option<T>,
/// }
/// # impl<T> From<T> for Alter<T> {
/// #     fn from(value: T) -> Self {
/// #         Self { value: Some(value) }
/// #     }
/// # }
/// # impl<T> From<Option<T>> for Alter<T> {
/// #     fn from(value: Option<T>) -> Self {
/// #         Self { value }
/// #     }
/// # }
/// # fn usize_with_side_effect() -> usize {
/// #     panic!()
/// # }
///
/// // Next we can use the `choice` macro with the name of the
/// // struct preceding by any value or fn call that can be
/// // converted to it via `From`.
/// // Note: A basic impl of `From<T>` and `From<Option<T>>` are
/// // given, but hidden in this example for the sake of brevity.
/// fn choose() -> Alter<usize> {
///     choice!(Alter, None, 1, usize_with_side_effect())
/// }
///
/// // The `choose` fn will then lazily try each of its alternatives,
/// // returning early on the first non neutral-matching value.
/// // Note: As `1` is the value found as an alternative,
/// // `usize_with_side_effect` is never called, because `choice` is lazy.
/// fn main() {
///     assert_eq!(choose().value, Some(1));
/// }
/// ```
#[macro_export]
macro_rules! choice {
    ($typ:ident, $( $x:expr ),* ; bail!($( $fmt:expr ),*)) => {{
        use $crate::__Alternative as _;
        $(
            $typ::__from($x)?;
        )*
        $crate::bail!($( $fmt ),*);
    }};
    ($typ:ident, $( $x:expr ),* $(,)?) => {{
        use $crate::__Alternative as _;
        let mut alternative;
        $(
            alternative = $typ::__from($x)?;
        )*
        alternative
    }}
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

/// A trait for giving a type [`bail`] and [`ensure`] macro support.
pub trait FlowControl:
    Sized
    + FromResidual<ControlFlow<Self, Infallible>>
    + FromResidual<Result<Infallible, anyhow::Error>>
{
    fn ensure(condition: bool, f: impl FnOnce() -> Self) -> ControlFlow<Self, ()> {
        match condition {
            true => ControlFlow::Continue(()),
            false => ControlFlow::Break(f()),
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
            _ => unreachable!(),
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
        T: Success,
        T: FromResidual<R>,
    {
        match self.branch() {
            ControlFlow::Continue(_) => ControlFlow::Break(<T as Success>::success_value()),
            ControlFlow::Break(residual) => ControlFlow::Break(T::from_residual(residual)),
        }
    }

    fn into_success<T, R>(self) -> ControlFlow<T, !>
    where
        Self: lazy::private::Sealed,
        Self: Try<Residual = R, Output = Self::Internal>,
        T: SuccessFrom<From = Self::Internal>,
        T: FromResidual<R>,
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

pub trait ShortCircuit<T>: lazy::private::Sealed
where
    T: FromResidual<ControlFlow<T, Infallible>>,
{
    /// Emulates [`ok_or`][Option::ok_or] for returning types that implements
    /// [`FlowControl`] or [`FromResidual<ControlFlow<Self, !>>`].
    ///
    /// Can be used on [`Result`], [`Option`] and [`bool`].
    fn or_return(self, f: impl FnOnce() -> T) -> ControlFlow<T, Self::Internal>;
}

impl<A: lazy::private::Sealed, T> ShortCircuit<T> for A
where
    T: FromResidual<ControlFlow<T, Infallible>>,
{
    fn or_return(self, f: impl FnOnce() -> T) -> ControlFlow<T, Self::Internal> {
        match self.value() {
            Some(v) => ControlFlow::Continue(v),
            None => ControlFlow::Break(f()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Display;

    use anyhow::{Error, Result};

    use crate as firelib;
    use crate::{lazy::*, *};

    pub enum ErrorType {
        Err(String),
        I32(i32),
    }

    #[derive(FlowControl)]
    #[alternative(value, Ok(None))]
    pub struct OptionErr<T> {
        pub value: LazyResult<Option<T>, ErrorType>,
    }

    impl<T> Default for OptionErr<T> {
        fn default() -> Self {
            Self { value: Ok(None) }
        }
    }

    impl<T> From<Error> for OptionErr<T> {
        fn from(err: Error) -> Self {
            Self {
                value: Err(LazyError::new(move |f| {
                    format!("{}", f.apply(ErrorType::Err(err.to_string())))
                })),
            }
        }
    }

    impl<T> FromResidual<Result<Infallible, LazyError<ErrorType>>> for OptionErr<T> {
        fn from_residual(residual: Result<Infallible, LazyError<ErrorType>>) -> Self {
            match residual {
                Err(err) => Self { value: Err(err) },
                Ok(_) => unreachable!(),
            }
        }
    }

    impl<T> OptionErr<T> {
        pub fn new(value: T) -> Self {
            Self { value: Ok(Some(value)) }
        }
    }

    impl<T: Display> Display for OptionErr<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self.value {
                Ok(Some(value)) => write!(f, "Some: {}", value),
                Ok(None) => write!(f, "None"),
                Err(err) => write!(f, "Error: {}", err.apply(&|error| format_err(error))),
            }
        }
    }

    #[test]
    fn test_error() {
        assert_eq!(choice1().to_string(), "Error: -> `69`");
    }

    #[test]
    fn test_some() {
        assert_eq!(choice2().to_string(), "Some: 420");
    }

    #[test]
    fn test_bail() {
        assert_eq!(choice3().to_string(), "Error: No alternative found");
    }

    fn choice1() -> OptionErr<i32> {
        choice!(OptionErr, value1(), value2(), value3(); bail!("No alternative found"));
    }

    fn choice2() -> OptionErr<i32> {
        choice!(OptionErr, value1(), value3(); bail!("No alternative found"));
    }

    fn choice3() -> OptionErr<i32> {
        choice!(OptionErr, value1(); bail!("No alternative found"));
    }

    fn value1() -> OptionErr<i32> {
        OptionErr::default()
    }

    fn value2() -> OptionErr<i32> {
        lazy_fail(69)?;

        OptionErr::default()
    }

    fn value3() -> OptionErr<i32> {
        OptionErr::new(420)
    }

    fn lazy_fail(i: i32) -> LazyResult<(), ErrorType> {
        lazybail!(|f| "-> {}", f.apply(ErrorType::I32(i)))
    }

    fn format_err(err: ErrorType) -> String {
        match err {
            ErrorType::Err(err) => err,
            ErrorType::I32(n) => format!("`{}`", n),
        }
    }
}
