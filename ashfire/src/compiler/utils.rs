use firelib::lexer::Loc;

use super::program::{Fmt, LazyError, LazyResult};

pub fn error_loc<S: ToString>(error: S, loc: Loc) -> LazyError {
    let error = error.to_string();
    LazyError::new(move |f| format!("{}{error}", f.format(Fmt::Loc(loc))))
}

#[track_caller]
pub fn todo(loc: Loc) -> LazyResult<()> {
    Err(error_loc(format!("\n[HERE]  {}", std::panic::Location::caller()), loc))
}
