use std::{
    any::type_name,
    convert::Infallible,
    fmt::{Debug, Display},
    ops::{ControlFlow, Try},
    string::ToString,
};

use anyhow::{Error, Result};
use itertools::Itertools;

pub type LazyResult<'err, R, T> = Result<R, LazyError<'err, T>>;

pub trait Formatter<T> {
    fn format(&self, t: T) -> String;
}

impl<T, A: Fn(T) -> String> Formatter<T> for A {
    fn format(&self, t: T) -> String {
        self(t)
    }
}

pub trait LazyFormatter<T> {
    fn apply(&self, t: &dyn Formatter<T>) -> String;
}

impl<T, A: Fn(&dyn Formatter<T>) -> String> LazyFormatter<T> for A {
    fn apply(&self, t: &dyn Formatter<T>) -> String {
        self(t)
    }
}

pub struct LazyError<'err, T>(Box<dyn for<'a> Formatter<&'a dyn Formatter<T>> + 'err>);

impl<'err, T> LazyError<'err, T> {
    pub fn new(lazy_error: impl Fn(&dyn Formatter<T>) -> String + 'err) -> Self {
        Self(Box::new(lazy_error))
    }

    pub fn apply(&self, formatter: &dyn Formatter<T>) -> Error {
        anyhow::anyhow!(self.0.format(formatter))
    }

    pub fn skip(&self) -> Error {
        anyhow::anyhow!(self.0.format(&|_| format!("<{}>", type_name::<T>())))
    }
}

impl<'err, T> Display for LazyError<'err, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.skip())
    }
}

impl<'err, T> Debug for LazyError<'err, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("LazyError").field(&self.skip()).finish()
    }
}

impl<'err, E> From<Error> for LazyError<'err, E> {
    fn from(value: Error) -> Self {
        Self::new(move |_| value.chain().map(ToString::to_string).join("\n"))
    }
}

pub trait LazyCtx<'err, T>: private::Sealed {
    fn with_ctx(
        self, lazy_error: impl Fn(&dyn Formatter<T>) -> String + 'err,
    ) -> Result<Self::Internal, LazyError<'err, T>>;

    fn with_err_ctx(
        self, lazy_error: impl Fn() -> LazyError<'err, T> + 'err,
    ) -> Result<Self::Internal, LazyError<'err, T>>;
}

impl<'err, A: private::Sealed, T> LazyCtx<'err, T> for A {
    fn with_ctx(
        self, lazy_error: impl Fn(&dyn Formatter<T>) -> String + 'err,
    ) -> Result<Self::Internal, LazyError<'err, T>> {
        match self.value() {
            Ok(output) => Ok(output),
            Err(err) => Err(if err.to_string().is_empty() {
                LazyError::new(lazy_error)
            } else {
                LazyError::new(move |f| format!("{}\n{err}", lazy_error.apply(f)))
            }),
        }
    }

    fn with_err_ctx(
        self, lazy_error: impl Fn() -> LazyError<'err, T> + 'err,
    ) -> Result<Self::Internal, LazyError<'err, T>> {
        match self.value() {
            Ok(output) => Ok(output),
            Err(err) => Err(if err.to_string().is_empty() {
                lazy_error()
            } else {
                LazyError::new(move |f| format!("{}\n{err}", lazy_error().apply(f)))
            }),
        }
    }
}

pub trait LazyErrCtx<'err, T>: private::SealedT<'err, T> {
    fn with_ctx(
        self, lazy_error: impl Fn(&dyn Formatter<T>) -> String + 'err,
    ) -> Result<Self::Internal, LazyError<'err, T>>;

    fn with_err_ctx(
        self, lazy_error: impl Fn() -> LazyError<'err, T> + 'err,
    ) -> Result<Self::Internal, LazyError<'err, T>>;

    fn try_or_apply(self, format: &dyn Formatter<T>) -> Result<Self::Internal>
    where
        Self: Try<Residual = Result<Infallible, LazyError<'err, T>>, Output = Self::Internal>;
}

impl<'err, R, T: 'err, A: private::SealedT<'err, T, Internal = R>> LazyErrCtx<'err, T> for A {
    fn with_ctx(
        self, lazy_error: impl Fn(&dyn Formatter<T>) -> String + 'err,
    ) -> Result<Self::Internal, LazyError<'err, T>> {
        match self.lazy_value() {
            Ok(output) => Ok(output),
            Err(err) => {
                Err(LazyError::new(move |f| format!("{}\n{}", lazy_error.apply(f), err.apply(f))))
            }
        }
    }

    fn with_err_ctx(
        self, lazy_error: impl Fn() -> LazyError<'err, T> + 'err,
    ) -> Result<Self::Internal, LazyError<'err, T>> {
        match self.lazy_value() {
            Ok(output) => Ok(output),
            Err(err) => {
                Err(LazyError::new(move |f| format!("{}\n{}", lazy_error().apply(f), err.apply(f))))
            }
        }
    }

    fn try_or_apply(self, format: &dyn Formatter<T>) -> Result<Self::Internal>
    where
        Self: Try<Residual = Result<Infallible, LazyError<'err, T>>, Output = Self::Internal>,
    {
        match self.branch() {
            ControlFlow::Continue(output) => Ok(output),
            ControlFlow::Break(lazy) => Err(lazy.unwrap_err().apply(format)),
        }
    }
}

#[macro_export]
macro_rules! lazybail {
    (| $i:pat_param | $($fmt:expr ),* ) => {
        Err($crate::lazyerr!(|$i| $($fmt),* ))?
    };
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
        $crate::lazy::LazyError::new(move |$i| {
            format!( $( $fmt ),* )
        })
    };
}

#[macro_export]
macro_rules! lazyformat {
    (| $i:pat_param | $($fmt:expr ),*) => {
        move |$i: &dyn $crate::lazy::Formatter<_>| format!( $( $fmt ),* )
    };
}

#[macro_export]
macro_rules! lazyformatter {
    (| $i:pat_param | $fmt:expr) => {
        move |$i: &dyn $crate::lazy::Formatter<_>| $fmt
    };
}

pub(crate) mod private {
    use anyhow::{Context, Result};

    use super::LazyResult;

    pub trait Sealed {
        type Internal;
        fn value(self) -> Result<Self::Internal>;
    }

    impl<T> Sealed for Result<T> {
        type Internal = T;

        fn value(self) -> Self {
            self
        }
    }

    impl<T> Sealed for Option<T> {
        type Internal = T;

        fn value(self) -> Result<T> {
            self.with_context(String::new)
        }
    }

    impl Sealed for bool {
        type Internal = Self;

        fn value(self) -> Result<Self> {
            self.then_some(true).with_context(String::new)
        }
    }

    pub trait SealedT<'err, T> {
        type Internal;

        fn lazy_value(self) -> LazyResult<'err, Self::Internal, T>;
    }

    impl<'err, R, T> SealedT<'err, T> for LazyResult<'err, R, T> {
        type Internal = R;

        fn lazy_value(self) -> LazyResult<'err, R, T> {
            self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{LazyCtx, LazyError, LazyResult};
    use crate::lazy::LazyErrCtx;

    #[test]
    fn lazyerr_test() {
        let lazyerr = |i| -> LazyResult<'_, (), i32> { Err(lazyerr!(|f| "{}", f.format(i))) };
        let err = lazyerr(69).try_or_apply(&int_error_fmt).unwrap_err();

        assert_eq!("Error: `69`", err.to_string());
    }

    #[test]
    fn ctx_test() {
        let err = None::<()>
            .with_ctx(|f| f.format(420))
            .try_or_apply(&int_error_fmt)
            .unwrap_err();

        assert_eq!("Error: `420`", err.to_string());
    }

    #[test]
    fn ctx_ok_test() {
        Some(())
            .with_ctx(|f| f.format(0))
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

    fn format_mutate_format(cls: &LazyError<i32>, program: &mut Program) -> String {
        let first = cls.apply(&|i| program.format(i));
        program.plus(10);
        let second = cls.apply(&|i| program.format(i));

        format!("{first}. {second}")
    }

    fn new_lazy_result<'err, V, T: Copy + 'err>(value: T) -> LazyResult<'err, V, T> {
        Err(new_lazy_err(value))
    }

    fn new_lazy_err<'err, T: Copy + 'err>(value: T) -> LazyError<'err, T> {
        lazyerr!(|f| "Value: {}", f.format(value))
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
