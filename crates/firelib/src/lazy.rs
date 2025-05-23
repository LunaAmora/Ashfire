use std::{
    any::type_name,
    convert::Infallible,
    fmt::{Debug, Display},
    ops::{ControlFlow, Try},
    string::ToString,
};

use itertools::Itertools;

pub type Result<'err, R, T> = anyhow::Result<R, Error<'err, T>>;

pub type Formatter<'a, T> = &'a dyn Fn(T) -> String;

pub trait LazyFormatter<T> {
    fn apply(&self, f: Formatter<'_, T>) -> String;
    fn is_empty(&self) -> bool {
        false
    }
}

impl<T, A: Fn(Formatter<'_, T>) -> String> LazyFormatter<T> for A {
    fn apply(&self, f: Formatter<'_, T>) -> String {
        self(f)
    }
}
impl<T> LazyFormatter<T> for Error<'_, T> {
    fn apply(&self, f: Formatter<'_, T>) -> String {
        self.0(f)
    }
}

impl<T> LazyFormatter<T> for anyhow::Error {
    fn apply(&self, _: Formatter<'_, T>) -> String {
        self.to_string()
    }

    fn is_empty(&self) -> bool {
        self.to_string().is_empty()
    }
}

pub trait LazyError<T> = Fn(Formatter<'_, T>) -> String;

pub struct Error<'err, T>(Box<dyn LazyError<T> + 'err>);

impl<'err, T> Error<'err, T> {
    pub fn new(lazy_error: impl LazyError<T> + 'err) -> Self {
        Self(Box::new(lazy_error))
    }

    pub fn skip(&self) -> anyhow::Error {
        anyhow::anyhow!(self.0(&|_| format!("<{}>", type_name::<T>())))
    }
}

impl<T> Display for Error<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.skip())
    }
}

impl<T> Debug for Error<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("LazyError").field(&self.skip()).finish()
    }
}

impl<E> From<anyhow::Error> for Error<'_, E> {
    fn from(value: anyhow::Error) -> Self {
        Self::new(move |_| value.chain().map(ToString::to_string).join("\n"))
    }
}

pub trait LazyCtx<'err, T>: private::Sealed {
    fn with_ctx(
        self, lazy_error: impl LazyError<T> + 'err,
    ) -> anyhow::Result<Self::Internal, Error<'err, T>>;

    fn with_err_ctx(
        self, lazy_error: impl Fn() -> Error<'err, T> + 'err,
    ) -> anyhow::Result<Self::Internal, Error<'err, T>>;

    fn try_or_apply(self, format: Formatter<'_, T>) -> anyhow::Result<Self::Internal>
    where
        Self: Try<Residual = anyhow::Result<Infallible, Error<'err, T>>, Output = Self::Internal>;
}

impl<'err, A: private::Sealed, T> LazyCtx<'err, T> for A
where
    A::Error: LazyFormatter<T> + 'err,
{
    fn with_ctx(
        self, lazy_error: impl LazyError<T> + 'err,
    ) -> anyhow::Result<Self::Internal, Error<'err, T>> {
        self.value().map_err(|err| {
            if err.is_empty() {
                Error::new(lazy_error)
            } else {
                Error::new(move |f| format!("{}\n{}", lazy_error.apply(f), err.apply(f)))
            }
        })
    }

    fn with_err_ctx(
        self, lazy_error: impl Fn() -> Error<'err, T> + 'err,
    ) -> anyhow::Result<Self::Internal, Error<'err, T>> {
        self.value().map_err(|err| {
            if err.is_empty() {
                lazy_error()
            } else {
                Error::new(move |f| format!("{}\n{}", lazy_error().apply(f), err.apply(f)))
            }
        })
    }

    fn try_or_apply(self, format: Formatter<'_, T>) -> anyhow::Result<Self::Internal>
    where
        Self: Try<Residual = anyhow::Result<Infallible, Error<'err, T>>, Output = Self::Internal>,
    {
        match self.branch() {
            ControlFlow::Continue(output) => Ok(output),
            ControlFlow::Break(Err(lazy)) => Err(anyhow::anyhow!(lazy.apply(format))),
        }
    }
}

#[macro_export]
macro_rules! lazybail {
    (| $i:pat_param | $($fmt:expr ),* ) => {{
        Err($crate::lazyerr!(|$i| $($fmt),* ))?;
        unreachable!();
    }};
}

#[macro_export]
macro_rules! lazyctx {
    (| $i:pat_param | $($fmt:expr ),* ) => {
        move || $crate::lazyerr!(|$i| $($fmt),* )
    };
}

#[macro_export]
macro_rules! lazyerr {
    (| $i:pat_param | $($fmt:expr ),* ) => {
        $crate::lazy::Error::new(move |$i| {
            format!( $( $fmt ),* )
        })
    };
}

#[macro_export]
macro_rules! lazyformat {
    (| $i:pat_param | $($fmt:expr ),*) => {
        move |$i: $crate::lazy::Formatter<_>| format!( $( $fmt ),* )
    };
}

#[macro_export]
macro_rules! lazyformatter {
    (| $i:pat_param | $fmt:expr) => {
        move |$i: $crate::lazy::Formatter<_>| $fmt
    };
}

pub(crate) mod private {
    use anyhow::{Context, Error};

    pub trait Sealed {
        type Error;
        type Internal;
        fn value(self) -> Result<Self::Internal, Self::Error>;
    }

    impl<T, E> Sealed for Result<T, E> {
        type Error = E;
        type Internal = T;

        fn value(self) -> Result<Self::Internal, Self::Error> {
            self
        }
    }

    impl<T> Sealed for Option<T> {
        type Error = Error;
        type Internal = T;

        fn value(self) -> Result<Self::Internal, Self::Error> {
            self.with_context(String::new)
        }
    }

    impl Sealed for bool {
        type Error = Error;
        type Internal = Self;

        fn value(self) -> Result<Self::Internal, Self::Error> {
            self.then_some(true).with_context(String::new)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Error, LazyCtx, Result};
    use crate::lazy::LazyFormatter;

    #[test]
    fn lazyerr_test() {
        let lazyerr = |i| -> Result<'_, (), i32> { Err(lazyerr!(|f| "{}", f(i))) };
        let err = lazyerr(69).try_or_apply(&int_error_fmt).unwrap_err();

        assert_eq!("Error: `69`", err.to_string());
    }

    #[test]
    fn ctx_test() {
        let err = None::<()>
            .with_ctx(|f| f(420))
            .try_or_apply(&int_error_fmt)
            .unwrap_err();

        assert_eq!("Error: `420`", err.to_string());
    }

    #[test]
    fn ctx_ok_test() {
        Some(())
            .with_ctx(|f| f(0))
            .try_or_apply(&int_error_fmt)
            .unwrap();
    }

    #[test]
    fn ctx_chain_test() {
        let err = new_lazy_result::<(), i32>(69)
            .with_err_ctx(|| new_lazy_err(420))
            .try_or_apply(&int_error_fmt)
            .unwrap_err();

        assert_eq!("Value: Error: `420`\nValue: Error: `69`", err.to_string());
    }

    fn int_error_fmt(i: i32) -> String {
        format!("Error: `{i}`")
    }

    #[test]
    fn lifetimes_test() {
        let a = format_mutate_format(&new_lazy_err(34), &mut Program(25));
        let b = format_mutate_format(&new_lazy_err(40), &mut Program(19));
        assert_eq!(a, b);
    }

    fn format_mutate_format(cls: &Error<i32>, program: &mut Program) -> String {
        let first = cls.apply(&|i| program.format(i));
        program.plus(10);
        let second = cls.apply(&|i| program.format(i));

        format!("{first}. {second}")
    }

    fn new_lazy_result<'err, V, T: Copy + 'err>(value: T) -> Result<'err, V, T> {
        Err(new_lazy_err(value))
    }

    fn new_lazy_err<'err, T: Copy + 'err>(value: T) -> Error<'err, T> {
        lazyerr!(|f| "Value: {}", f(value))
    }

    struct Program(i32);

    impl Program {
        fn format(&self, i: i32) -> String {
            format!("`{}`", self.0 + i)
        }

        fn plus(&mut self, i: i32) {
            self.0 += i;
        }
    }
}
