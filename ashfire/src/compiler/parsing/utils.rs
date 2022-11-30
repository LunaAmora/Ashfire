use std::fmt::Display;

use ashfire_types::{
    core::{IRToken, Op, TokenType},
    enums::KeywordType,
};
use firelib::lexer::Loc;

use super::{parser::Parser, types::LocWord};
use crate::compiler::program::{Fmt, LazyError, LazyResult};

impl Parser {
    pub fn expect_keyword<S: Display + 'static>(
        &mut self, key: KeywordType, error_text: S, loc: Loc,
    ) -> LazyResult<IRToken> {
        self.expect_by(|tok| tok == key, error_text, loc)
    }

    pub fn expect_word<S: Display + 'static>(
        &mut self, error_text: S, loc: Loc,
    ) -> LazyResult<LocWord> {
        self.expect_by(|tok| tok == TokenType::Word, error_text, loc)
            .map(|tok| LocWord::new(&tok, tok.loc))
    }

    pub fn expect_by<S: Display + 'static>(
        &mut self, pred: impl FnOnce(&IRToken) -> bool, error_text: S, loc: Loc,
    ) -> LazyResult<IRToken> {
        expect_token_by(self.next(), pred, error_text, loc)
    }
}

pub fn invalid_option<S: Display + 'static>(tok: Option<IRToken>, desc: S, loc: Loc) -> LazyError {
    if let Some(tok) = tok {
        unexpected_token(tok, desc)
    } else {
        unexpected_end(desc, loc)
    }
}

pub fn unexpected_end<S: Display + 'static>(desc: S, loc: Loc) -> LazyError {
    LazyError::new(move |f| {
        format!("{}Expected {desc}, but found nothing", f.format(Fmt::Loc(loc)))
    })
}

pub fn unexpected_token<S: Display + 'static>(tok: IRToken, desc: S) -> LazyError {
    let IRToken { token_type, loc, .. } = tok;

    LazyError::new(move |f| {
        format!(
            "{}Expected {desc}, but found: {} `{}`",
            f.format(Fmt::Loc(loc)),
            f.format(Fmt::Typ(token_type)),
            f.format(Fmt::Tok(tok.clone()))
        )
    })
}

pub fn invalid_token<S: Display + 'static>(tok: IRToken, error: S) -> LazyError {
    let IRToken { token_type, loc, .. } = tok;

    LazyError::new(move |f| {
        format!(
            "{}Invalid `{}` found on {error}: `{}`",
            f.format(Fmt::Loc(loc)),
            f.format(Fmt::Typ(token_type)),
            f.format(Fmt::Tok(tok.clone()))
        )
    })
}

pub fn format_block<S: Display + 'static>(error: S, op: &Op, loc: Loc) -> LazyError {
    let (op_loc, typ) = (op.loc, op.op_type);
    LazyError::new(move |f| {
        format!(
            concat!(
                "{}{}, but found a `{:?}` block instead\n",
                "[INFO] {}The found block started here."
            ),
            f.format(Fmt::Loc(loc)),
            error,
            typ,
            f.format(Fmt::Loc(op_loc))
        )
    })
}

pub fn expect_token_by<S: Display + 'static>(
    value: Option<IRToken>, pred: impl FnOnce(&IRToken) -> bool, desc: S, loc: Loc,
) -> LazyResult<IRToken> {
    match value {
        Some(tok) if pred(&tok) => Ok(tok),
        invalid => Err(invalid_option(invalid, desc, loc)),
    }
}
