#![feature(try_trait_v2)]
#![feature(never_type)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
    path::Path,
};

pub use anyhow;
pub use firelib_macro::{alternative, FlowControl};

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

/// Provides the [`success`][Success::success] method
/// that returns a default-like success value.
///
/// A trait for giving a type [`success`] macro support.
pub trait Success {
    fn success() -> Self;
}

/// Provides the [`success_from`][SuccessFrom::success_from] method
/// that pass a value to a `SuccessFrom` constructor.
///
/// A trait for giving a type [`success_from`] macro support.
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

    fn short_circuit(condition: bool) -> ControlFlow<Self, ()>
    where
        Self: Default,
    {
        Self::ensure(!condition, Self::default)
    }

    fn flow_success() -> ControlFlow<Self, !>
    where
        Self: Success,
    {
        ControlFlow::Break(<Self as Success>::success())
    }

    fn flow_success_from(from: <Self as SuccessFrom>::From) -> ControlFlow<Self, !>
    where
        Self: SuccessFrom,
    {
        ControlFlow::Break(<Self as SuccessFrom>::success_from(from))
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

/// Checks the condition to ensure it is true.
/// Otherwise returns early with the [`Default`]
/// value of the type that implements [`FlowControl`].
#[macro_export]
macro_rules! short_circuit {
    ($cond:expr) => {
        $crate::FlowControl::short_circuit($cond)?
    };
}

/// Returns early with the [`Success`][Success::success] impl of the returning type.
#[macro_export]
macro_rules! success {
    () => {
        $crate::FlowControl::flow_success()?;
    };
    ($expr:expr) => {{
        $expr?;
        success!();
    }};
}

/// Returns early with the [`SuccessFrom`][SuccessFrom::success_from] impl of the returning type.
#[macro_export]
macro_rules! success_from {
    ($expr:expr) => {
        $crate::FlowControl::flow_success_from($expr?)?;
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

/// Gets the directory of the [`Path`],
/// or [`None`] if is empty.
pub fn get_dir(current: &Path) -> Option<&Path> {
    current.ancestors().nth(1)
}

/// Maps an empty vector to [`None`], and a non empty to [`Some<Vec<T>>`].
pub fn empty_or_some<T>(vec: Vec<T>) -> Option<Vec<T>> {
    if vec.is_empty() {
        None
    } else {
        Some(vec)
    }
}

/// Consumes and flatten a nested [`Vec`].
pub fn flatten<T>(vec: Vec<Vec<T>>) -> Vec<T> {
    vec.into_iter().flatten().collect()
}

/// Push a value `T` to different [`Vec<T>`] based on the given condition.
pub fn push_by_condition<T>(cond: bool, value: T, if_true: &mut Vec<T>, if_false: &mut Vec<T>) {
    match cond {
        true => if_true.push(value),
        _ => if_false.push(value),
    }
}

/// Gets the index in the [`Vec`] that matches the given predicate.
///
/// # Panics
/// Panics if no matching element is found.
pub fn expect_index<T>(vec: &[T], pred: impl FnMut(&T) -> bool) -> usize {
    vec.iter()
        .position(pred)
        .expect("no item matched the given predicate")
}
