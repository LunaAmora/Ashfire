use std::fmt::Display;

use firelib::lexer::Loc;

use super::program::{Fmt, LazyError, Program};

pub fn err_loc<S: Display + 'static>(error: S, loc: Loc) -> LazyError {
    LazyError::new(move |f| format!("{}{error}", f.format(Fmt::Loc(loc))))
}

#[track_caller]
pub fn todo(loc: Loc) -> LazyError {
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
