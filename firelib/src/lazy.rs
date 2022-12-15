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

impl<T> std::error::Error for LazyError<'_, T> {}
unsafe impl<T> Send for LazyError<'_, T> {}
unsafe impl<T> Sync for LazyError<'_, T> {}

pub trait LazyCtx<'err, T>: private::Sealed {
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
    ( |$i:pat_param| $($fmt:expr ),* ) => {
        Err($crate::lazy::LazyError::new(move |$i| {
            format!( $( $fmt ),* )
        }))?
    };
}

#[macro_export]
macro_rules! lazyformat {
    (| $i:pat_param | $fmt:expr) => {
        move |$i: &dyn $crate::lazy::Formatter<_>| $fmt
    };
}

pub(crate) mod private {
    use anyhow::{Context, Error, Result};

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

    impl<R, T: 'static> Sealed for LazyResult<'static, R, T> {
        type Internal = R;

        fn value(self) -> Result<R> {
            self.map_err(Error::from)
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
}

#[cfg(test)]
mod tests {
    use super::{LazyCtx, LazyError, LazyResult};

    #[test]
    fn lazybail_test() {
        let err = lazybail_error(69).try_or_apply(&int_error_fmt).unwrap_err();

        assert_eq!("Error: `69`", err.to_string());
    }

    #[test]
    fn ctx_test() {
        let err = with_ctx_error(None, 420)
            .try_or_apply(&int_error_fmt)
            .unwrap_err();

        assert_eq!("Error: `420`", err.to_string());
    }

    #[test]
    fn ctx_ok_test() {
        with_ctx_error(Some(true), 0)
            .try_or_apply(&int_error_fmt)
            .unwrap();
    }

    fn with_ctx_error(option: Option<bool>, err_val: i32) -> LazyResult<'static, bool, i32> {
        let unwraped_option: bool = option.with_ctx(move |f| f.format(err_val))?;

        Ok(unwraped_option)
    }

    fn lazybail_error(i: i32) -> LazyResult<'static, bool, i32> {
        lazybail!(|f| "{}", f.format(i))
    }

    fn int_error_fmt(i: i32) -> String {
        format!("Error: `{i}`")
    }

    #[test]
    fn lifetimes_test() {
        let a = format_mutate_format(&create_cls(34), &mut Program(25));
        let b = format_mutate_format(&create_cls(40), &mut Program(19));
        assert_eq!(a, b);
    }

    fn format_mutate_format(cls: &LazyError<i32>, program: &mut Program) -> String {
        let first = cls.apply(&|i| program.format(i));
        program.plus(10);
        let second = cls.apply(&|i| program.format(i));

        format!("{first}. {second}")
    }

    fn create_cls<'err, T: Copy + 'err>(value: T) -> LazyError<'err, T> {
        LazyError::new(move |f| format!("Value: {}", f.format(value)))
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
