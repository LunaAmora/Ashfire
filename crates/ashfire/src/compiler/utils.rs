use std::fmt::Display;

use firelib::{lazy, lexer::Loc};

use super::program::{Fmt, Program};

pub type OptionErr<'err, T, E = Fmt> = ashlib::OptionErr<'err, T, E>;
pub type LazyResult<'err, T, E = Fmt> = lazy::LazyResult<'err, T, E>;
pub type LazyError<'err, E = Fmt> = lazy::LazyError<'err, E>;

pub fn err_loc<'err, S: Display + 'err>(error: S, loc: Loc) -> LazyError<'err> {
    lazyerr!(|f| "{}{}", f.format(Fmt::Loc(loc)), error)
}

#[track_caller]
pub fn todo<'err>(loc: Loc) -> LazyError<'err> {
    err_loc(format!("\n[HERE]  {}", std::panic::Location::caller()), loc)
}

impl Program {
    #[track_caller]
    pub fn todo<S: Display>(&self, loc: Loc, error: S) -> ! {
        panic!("{}{error}", self.loc_fmt(loc))
    }

    #[track_caller]
    pub fn info_here<S: Display>(&self, loc: Loc, info: S) {
        info!("{}{info}\n[HERE]  {}", self.loc_fmt(loc), std::panic::Location::caller());
    }

    pub fn info<S: Display>(&self, loc: Loc, info: S) {
        info!("{}{info}", self.loc_fmt(loc));
    }
}
