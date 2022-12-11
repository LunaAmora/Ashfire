use std::fmt::Display;

use ashfire_types::{
    core::{IRToken, Location, TokenType},
    data::TypeId,
    enums::{ControlOp, KeywordType, OpType},
};
use firelib::lexer::Loc;

use super::{parser::Parser, types::LocWord};
use crate::compiler::program::{Fmt, LazyError, LazyResult, Program};

pub struct LabelKind(pub LocWord, pub Option<TypeId>);

impl Parser {
    pub fn expect_label_kind<S: Display + 'static + Clone>(
        &mut self, error_text: S, loc: Loc, prog: &mut Program,
    ) -> LazyResult<LabelKind> {
        let word = self.expect_word(error_text.clone(), loc)?;

        let maybe_typed = match self.peek() {
            Some(tok) => match tok.get_keyword() {
                Some(KeywordType::Colon) => {
                    self.next();
                    self.expect_type_kind(error_text, prog, loc).map(Some)?
                }
                _ => None,
            },
            None => return Err(unexpected_end(error_text, loc)),
        };

        Ok(LabelKind(word, maybe_typed))
    }

    pub fn expect_type_kind<S: Display + 'static + Clone>(
        &mut self, error_text: S, prog: &mut Program, loc: Loc,
    ) -> LazyResult<TypeId> {
        let next = self.expect_by(
            |tok| equals_any!(tok, KeywordType::Ref, TokenType::Word),
            error_text.clone(),
            loc,
        )?;

        self.check_type_kind(next, error_text, prog)
    }

    pub fn check_type_kind<S: Display + 'static + Clone>(
        &mut self, tok: IRToken, error_text: S, prog: &mut Program,
    ) -> LazyResult<TypeId> {
        let IRToken(token_type, _, loc) = tok;
        match token_type {
            TokenType::Keyword => {
                let word_error = format!("{error_text} after `*`");
                let ref_word = self.expect_word(word_error.clone(), loc)?;

                prog.get_type_id(ref_word.name())
                    .map(|x| prog.get_type_ptr(x))
                    .map_or_else(|| Err(unexpected_token(ref_word.into(), word_error)), Ok)
            }

            TokenType::Word => {
                let name_type = LocWord(tok.name(), loc);

                prog.get_type_id(name_type.name())
                    .map_or_else(|| Err(unexpected_token(name_type.into(), error_text)), Ok)
            }

            _ => Err(unexpected_token(tok, error_text)),
        }
    }

    pub fn expect_keyword<S: Display + 'static>(
        &mut self, key: KeywordType, error_text: S, loc: Loc,
    ) -> LazyResult<IRToken> {
        self.expect_by(|tok| tok == key, error_text, loc)
    }

    pub fn expect_word<S: Display + 'static>(
        &mut self, error_text: S, loc: Loc,
    ) -> LazyResult<LocWord> {
        self.expect_by(|tok| tok == TokenType::Word, error_text, loc)
            .map(|tok| LocWord(tok.name(), tok.loc()))
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
    let IRToken(token_type, _, loc) = tok;

    LazyError::new(move |f| {
        format!(
            "{}Expected {desc}, but found: {} `{}`",
            f.format(Fmt::Loc(loc)),
            f.format(Fmt::Typ(token_type)),
            f.format(Fmt::Tok(tok.clone()))
        )
    })
}

pub fn invalid_context(tok: IRToken, word: &str) -> LazyError {
    invalid_token(tok, format!("`{word}` context declaration"))
}

pub fn invalid_token<S: Display + 'static>(tok: IRToken, error: S) -> LazyError {
    let IRToken(token_type, _, loc) = tok;

    LazyError::new(move |f| {
        format!(
            "{}Invalid `{}` found on {error}: `{}`",
            f.format(Fmt::Loc(loc)),
            f.format(Fmt::Typ(token_type)),
            f.format(Fmt::Tok(tok.clone()))
        )
    })
}

pub fn format_block<S: Display + 'static>(
    error: S, op: (ControlOp, usize, Loc), loc: Loc,
) -> LazyError {
    LazyError::new(move |f| {
        format!(
            concat!(
                "{}{}, but found a `{:?}` block instead\n",
                "[INFO] {}The found block started here."
            ),
            f.format(Fmt::Loc(loc)),
            error,
            OpType::ControlOp(op.0, op.1),
            f.format(Fmt::Loc(op.2))
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
