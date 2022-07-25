#![feature(try_trait_v2)]
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
    path::Path,
};

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

/// Chain `Alternative::from` calls
/// in a short_circuit way with the try trait.
///
/// # See also
/// The choice function for Alternatives in haskell.
#[macro_export]
macro_rules! choice {
    ($typ:ident, $( $x:expr ),* $(,)?) => {
        {
            use $crate::__Alternative as _;
            let mut alternative;
            $(
                alternative = $typ::__from($x)?;
            )*
            alternative
        }
    }
}

pub trait Success {
    fn success() -> Self;
}

pub trait SucessFrom {
    type From;
    fn success_from(from: Self::From) -> Self;
}

pub trait FlowControl: Sized + FromResidual<ControlFlow<Self, Infallible>> {
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

    fn success() -> ControlFlow<Self, Infallible>
    where
        Self: Success,
    {
        ControlFlow::Break(<Self as Success>::success())
    }

    fn success_from(from: <Self as SucessFrom>::From) -> ControlFlow<Self, Infallible>
    where
        Self: SucessFrom,
    {
        ControlFlow::Break(<Self as SucessFrom>::success_from(from))
    }

    #[doc(hidden)]
    fn __from_residual(residual: ControlFlow<Self, Infallible>) -> Self {
        match residual {
            ControlFlow::Break(value) => value,
            _ => unreachable!(),
        }
    }
}

#[macro_export]
macro_rules! ensure {
    ($expr:expr, $( $fmt:expr ),*) => {
        $crate::FlowControl::ensure($expr,
            || {
                Err(anyhow::anyhow!( $( $fmt ),* ))?
            }
        )?
    };
}

#[macro_export]
macro_rules! bail {
    ($( $fmt:expr ),*) => {
        Err(anyhow::anyhow!( $( $fmt ),* ))?
    };
}

#[macro_export]
macro_rules! short_circuit {
    ($cond:expr) => {
        $crate::FlowControl::short_circuit($cond)?
    };
}

#[macro_export]
macro_rules! success {
    () => {
        $crate::FlowControl::success()?;
        unreachable!()
    };
    ($expr:expr) => {{
        $expr?;
        success!();
    }};
}

#[macro_export]
macro_rules! success_from {
    ($expr:expr) => {{
        $crate::FlowControl::success_from($expr?)?;
        unreachable!();
    }};
}

#[macro_export]
macro_rules! equals_any {
    ($expression:expr, $( $equal:expr ),+) => {
        ($($expression == $equal)||*)
    };
}

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

pub fn get_dir(current: &Path) -> Option<&Path> {
    current.ancestors().nth(1)
}

pub fn empty_or_some<T>(vec: Vec<T>) -> Option<Vec<T>> {
    if vec.is_empty() {
        None
    } else {
        Some(vec)
    }
}

pub fn flatten<T>(vec: Vec<Vec<T>>) -> Vec<T> {
    vec.into_iter().flatten().collect()
}

pub fn push_by_condition<T>(cond: bool, value: T, if_true: &mut Vec<T>, if_false: &mut Vec<T>) {
    match cond {
        true => if_true.push(value),
        _ => if_false.push(value),
    }
}

pub fn expect_index<T>(vec: &[T], pred: impl FnMut(&T) -> bool) -> usize {
    vec.iter()
        .position(pred)
        .expect("no item matched the given predicate")
}

pub fn expect_get<T>(vec: &[T], index: usize) -> &T {
    vec.get(index).expect("index out of bounds")
}
