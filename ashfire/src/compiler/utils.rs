use firelib::lexer::Loc;

use super::program::{Fmt, LazyError, Program};

pub fn err_loc<S: ToString>(error: S, loc: Loc) -> LazyError {
    let error = error.to_string();
    LazyError::new(move |f| format!("{}{error}", f.format(Fmt::Loc(loc))))
}

#[track_caller]
pub fn todo(loc: Loc) -> LazyError {
    err_loc(format!("\n[HERE]  {}", std::panic::Location::caller()), loc)
}

impl Program {
    #[track_caller]
    pub fn todo<S: ToString>(&self, loc: Loc, error: S) -> ! {
        panic!("{}{}", self.loc_fmt(loc), error.to_string(),)
    }

    #[track_caller]
    pub fn info_here<S: ToString>(&self, loc: Loc, info: S) {
        info!(
            "{}{}\n[HERE]  {}",
            self.loc_fmt(loc),
            info.to_string(),
            std::panic::Location::caller()
        );
    }

    pub fn info<S: ToString>(&self, loc: Loc, info: S) {
        info!("{}{}", self.loc_fmt(loc), info.to_string());
    }
}
