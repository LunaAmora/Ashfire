use std::ops::Deref;

use ashfire_types::{core::*, data::DataType};
use ashlib::UncheckedStack;
use firelib::{lazy::LazyFormatter, lexer::Loc};
use itertools::Itertools;

use crate::compiler::{
    program::Fmt,
    utils::{LazyError, LazyResult},
};

pub enum ArityType<T: Copy> {
    Any,
    Same,
    Type(T),
}

/// Safe wrapper for [`UncheckedStack`] with proper errors messages.
pub trait Expect<'err, T: Clone + Typed + Location + 'err>: UncheckedStack<T> {
    fn expect_exact_pop(&mut self, contract: &[DataType], loc: Loc) -> LazyResult<'err, ()> {
        self.expect_exact(contract, loc)?;
        self.truncate(contract.len());
        Ok(())
    }

    fn expect_contract_pop(&mut self, contract: &[DataType], loc: Loc) -> LazyResult<'err, ()> {
        self.expect_arity(contract, loc)?;
        self.truncate(contract.len());
        Ok(())
    }

    fn expect_array_pop<const N: usize>(
        &mut self, contract: [DataType; N], loc: Loc,
    ) -> LazyResult<'err, [T; N]> {
        self.expect_arity(&contract, loc)?;
        Ok(self.pop_array())
    }

    fn expect_arity_pop<const N: usize>(
        &mut self, arity: ArityType<DataType>, loc: Loc,
    ) -> LazyResult<'err, [T; N]> {
        self.expect_arity_type::<N>(arity, loc)?;
        Ok(self.pop_array())
    }

    fn expect_peek(&mut self, arity_t: ArityType<DataType>, loc: Loc) -> LazyResult<'err, &T> {
        self.expect_arity_type::<1>(arity_t, loc)?;
        Ok(self.peek())
    }

    fn expect_pop(&mut self, loc: Loc) -> LazyResult<'err, T> {
        self.expect_stack_size(1, loc)?;
        Ok(self.pop())
    }

    fn expect_pop_n<const N: usize>(&mut self, loc: Loc) -> LazyResult<'err, [T; N]> {
        self.expect_stack_size(N, loc)?;
        Ok(self.pop_array())
    }

    fn expect_pop_type(&mut self, arity_t: DataType, loc: Loc) -> LazyResult<'err, T> {
        self.expect_arity_type::<1>(ArityType::Type(arity_t), loc)?;
        Ok(self.pop())
    }

    fn expect_arity_type<const N: usize>(
        &self, arity: ArityType<DataType>, loc: Loc,
    ) -> LazyResult<'err, ()> {
        self.expect_stack_size(N, loc)?;

        let typ = match arity {
            ArityType::Any => return Ok(()),
            ArityType::Same => self.get_from_top(0).get_type(),
            ArityType::Type(typ) => typ,
        };

        self.expect_arity(&[typ; N], loc)
    }

    fn expect_stack_size(&self, n: usize, loc: Loc) -> LazyResult<'err, ()> {
        let len = self.len();
        if len < n {
            lazybail!(
                |f| concat!(
                    "Stack has less elements than expected\n",
                    "[INFO] {}Expected `{}` elements, but found `{}`"
                ),
                f.format(Fmt::Loc(loc)),
                n,
                len
            );
        };
        Ok(())
    }

    fn pop_push<const N: usize>(
        &mut self, contr: [DataType; N], to_push: T, loc: Loc,
    ) -> LazyResult<'err, ()> {
        self.expect_array_pop(contr, loc)?;
        self.push(to_push);
        Ok(())
    }

    fn pop_push_arity<const N: usize, F>(
        &mut self, f: F, arity_t: ArityType<DataType>, loc: Loc,
    ) -> LazyResult<'err, ()>
    where
        F: Fn([T; N]) -> T,
    {
        let popped = self.expect_arity_pop(arity_t, loc)?;
        self.push(f(popped));
        Ok(())
    }

    fn pop_extend<const N: usize, const M: usize, F>(
        &mut self, f: F, loc: Loc,
    ) -> LazyResult<'err, ()>
    where
        F: Fn([T; N]) -> [T; M],
    {
        let popped = self.expect_pop_n(loc)?;
        self.extend(f(popped));
        Ok(())
    }
}

impl<'err, T: Clone + Typed + Location + 'err, X: Expect<'err, T> + ?Sized> Compare<'err, T> for X {}

pub trait Compare<'err, T: Clone + Typed + Location + 'err>: Deref<Target = [T]> {
    fn expect_exact<V: Typed>(&self, contract: &[V], loc: Loc) -> LazyResult<'err, ()> {
        if self.len() == contract.len() && self.expect_arity(contract, loc).is_ok() {
            return Ok(());
        }
        Err(self.format_stack_diff(contract, loc))
    }

    fn expect_arity<V: Typed>(&self, contract: &[V], loc: Loc) -> LazyResult<'err, ()> {
        if self.len() < contract.len() {
            return Err(self.format_stack_diff(contract, loc));
        }

        for (stk, contr) in self.iter().rev().zip(contract.iter().rev()) {
            if expect_type(stk, contr, loc).is_err() {
                return Err(self.format_stack_diff(contract, loc));
            }
        }

        Ok(())
    }

    fn format_stack_diff<V: Typed>(&self, contract: &[V], loc: Loc) -> LazyError<'err> {
        let contr = format_stack(contract);
        let stack = format_stack(self);
        let frame = format_frames(self);

        lazyerr!(
            |f| concat!(
                "Found stack does not match the expected types:\n",
                "[INFO] {}Expected types: {}\n",
                "[INFO] {}Actual types:   {}\n{}"
            ),
            f.format(Fmt::Loc(loc)),
            contr.apply(f),
            f.format(Fmt::Loc(loc)),
            stack.apply(f),
            frame.apply(f)
        )
    }
}

pub fn expect_type<'err, T: Clone + Typed + Location + 'err, V: Typed>(
    frame: &T, expected: V, loc: Loc,
) -> LazyResult<'err, ()> {
    let expected_type = expected.get_type();
    // Todo: Improve this equality check
    if equals_any!(expected_type, ANY, ANY_PTR, frame.get_type()) {
        return Ok(());
    }
    Err(format_type_diff(frame.clone(), expected.get_type(), loc))
}

fn format_type_diff<'err, T: Clone + Typed + Location + 'err>(
    frame: T, expected: DataType, loc: Loc,
) -> LazyError<'err> {
    lazyerr!(
        |f| "{}Expected type `{}`, but found `{}`\n{}",
        f.format(Fmt::Loc(loc)),
        f.format(Fmt::Dat(expected)),
        f.format(Fmt::Dat(frame.get_type())),
        format_frame(&frame).apply(f)
    )
}

pub fn format_frames<T: Clone + Typed + Location>(stack: &[T]) -> impl LazyFormatter<Fmt> {
    let copied = stack.to_vec();
    lazyformatter!(|f| copied.iter().map(|t| format_frame(t).apply(f)).join("\n"))
}

pub fn format_frame<T: Typed + Location>(t: T) -> impl LazyFormatter<Fmt> {
    lazyformat!(
        |f| "[INFO] {}Type `{}` was declared here",
        f.format(Fmt::Loc(t.loc())),
        f.format(Fmt::Dat(t.get_type()))
    )
}

pub fn format_stack<'err, T: Typed>(stack: &[T]) -> impl LazyFormatter<Fmt> + 'err {
    let types: Vec<DataType> = stack.iter().map(Typed::get_type).collect();
    lazyformat!(
        |f| "[{}] ->",
        types
            .iter()
            .map(|&t| format!("<{}>", f.format(Fmt::Dat(t))))
            .join(", ")
    )
}
