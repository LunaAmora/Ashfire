use std::fmt::Display;

use ashfire_types::{
    core::{IRToken, IRTokenExt, Location, TokenType},
    data::DataType,
    enums::{KeywordType, OpType},
};
use firelib::{lexer::Loc, span::Spanned};

use super::{
    parser::Parser,
    types::{Block, LocWord},
};
use crate::compiler::{
    ctx::{Ctx, Fmt},
    utils::{LazyError, LazyResult},
};

pub struct LabelKind(pub LocWord, pub Option<DataType>);

pub trait ExpectToken<'err, S: Display + 'err> {
    fn expect_label_kind(
        &mut self, error_text: S, loc: Loc, ctx: &mut Ctx,
    ) -> LazyResult<'err, LabelKind>;

    fn expect_type_kind(
        &mut self, error_text: S, ctx: &mut Ctx, loc: Loc,
    ) -> LazyResult<'err, DataType>;

    fn check_type_kind(
        &mut self, tok: IRToken, error_text: S, ctx: &mut Ctx,
    ) -> LazyResult<'err, DataType>;

    fn expect_keyword(
        &mut self, key: KeywordType, error_text: S, loc: Loc,
    ) -> LazyResult<'err, IRToken>;

    fn expect_word(&mut self, error_text: S, loc: Loc) -> LazyResult<'err, LocWord>;

    fn expect_by(
        &mut self, pred: impl FnOnce(&IRToken) -> bool, error_text: S, loc: Loc,
    ) -> LazyResult<'err, IRToken>;

    fn expect_by_option<T>(
        &mut self, pred: impl FnOnce(&IRToken) -> Option<T>, error_text: S, loc: Loc,
    ) -> LazyResult<'err, Spanned<T>>;
}

impl<'err, S: Display + 'err> ExpectToken<'err, S> for Parser {
    fn expect_label_kind(
        &mut self, error_text: S, loc: Loc, ctx: &mut Ctx,
    ) -> LazyResult<'err, LabelKind> {
        let word = self.expect_word(error_text.to_string(), loc)?;

        let maybe_typed = match self.peek() {
            Some(tok) => match tok.get_keyword() {
                Some(KeywordType::Colon) => {
                    self.next();
                    self.expect_type_kind(error_text, ctx, loc).map(Some)?
                }
                _ => None,
            },
            None => return Err(unexpected_end(error_text, loc)),
        };

        Ok(LabelKind(word, maybe_typed))
    }

    fn expect_type_kind(
        &mut self, error_text: S, ctx: &mut Ctx, loc: Loc,
    ) -> LazyResult<'err, DataType> {
        let next = self.expect_by(
            |tok| tok == KeywordType::Ref || tok.get_word().is_some(),
            error_text.to_string(),
            loc,
        )?;

        self.check_type_kind(next, error_text, ctx)
    }

    fn check_type_kind(
        &mut self, tok: IRToken, error_text: S, ctx: &mut Ctx,
    ) -> LazyResult<'err, DataType> {
        let (token_type, loc) = tok;
        match token_type {
            TokenType::Keyword(_) => {
                let word_error = format!("{error_text} after `*`");
                let ref_word @ (name, _) = self.expect_word(word_error.clone(), loc)?;

                ctx.get_data_type(name)
                    .map(|x| ctx.get_type_ptr(x))
                    .map_or_else(|| Err(unexpected_token(ref_word, word_error)), Ok)
            }

            TokenType::Word(name) => ctx
                .get_data_type(name)
                .map_or_else(|| Err(unexpected_token((name, loc), error_text)), Ok),

            _ => Err(unexpected_token(tok, error_text)),
        }
    }

    fn expect_keyword(
        &mut self, key: KeywordType, error_text: S, loc: Loc,
    ) -> LazyResult<'err, IRToken> {
        self.expect_by(|tok| tok == key, error_text, loc)
    }

    fn expect_word(&mut self, error_text: S, loc: Loc) -> LazyResult<'err, LocWord> {
        self.expect_by_option(IRTokenExt::get_word, error_text, loc)
    }

    fn expect_by(
        &mut self, pred: impl FnOnce(&IRToken) -> bool, error_text: S, loc: Loc,
    ) -> LazyResult<'err, IRToken> {
        expect_token_by(self.next(), pred, error_text, loc)
    }

    fn expect_by_option<T>(
        &mut self, pred: impl FnOnce(&IRToken) -> Option<T>, error_text: S, loc: Loc,
    ) -> LazyResult<'err, Spanned<T>> {
        expect_token_by_option(self.next(), pred, error_text, loc)
    }
}

pub fn invalid_option<'err, S: Display + 'err>(
    tok: Option<IRToken>, desc: S, loc: Loc,
) -> LazyError<'err> {
    if let Some(tok) = tok {
        unexpected_token(tok, desc)
    } else {
        unexpected_end(desc, loc)
    }
}

pub fn unexpected_end<'err, S: Display + 'err>(desc: S, loc: Loc) -> LazyError<'err> {
    lazyerr!(|f| "{}Expected {desc}, but found nothing", f.format(Fmt::Loc(loc)))
}

pub fn unexpected_token<'err, S: Display + 'err, T>(
    (t, loc): Spanned<T>, desc: S,
) -> LazyError<'err>
where
    TokenType: From<T>,
{
    let tok @ (token_type, loc) = (t.into(), loc);

    lazyerr!(
        |f| "{}Expected {desc}, but found: {} `{}`",
        f.format(Fmt::Loc(loc)),
        f.format(Fmt::Typ(token_type)),
        f.format(Fmt::Tok(tok))
    )
}

pub fn invalid_context<'err, S: Display>(tok: IRToken, word: S) -> LazyError<'err> {
    invalid_token(tok, format!("`{}` context declaration", word))
}

pub fn invalid_token<'err, S: Display + 'err>(tok: IRToken, error: S) -> LazyError<'err> {
    let (token_type, loc) = tok;
    lazyerr!(
        |f| "{}Invalid `{}` found on {error}: `{}`",
        f.format(Fmt::Loc(loc)),
        f.format(Fmt::Typ(token_type)),
        f.format(Fmt::Tok(tok))
    )
}

pub fn format_block<'err, S: Display + 'err>(
    error: S, (control, value, start_loc): Block, loc: Loc,
) -> LazyError<'err> {
    lazyerr!(
        |f| concat!(
            "{}{}, but found a `{:?}` block instead\n",
            "[INFO] {}The found block started here."
        ),
        f.format(Fmt::Loc(loc)),
        error,
        OpType::ControlOp(control, value),
        f.format(Fmt::Loc(start_loc))
    )
}

pub fn expect_token_by<'err, S: Display + 'err>(
    value: Option<IRToken>, pred: impl FnOnce(&IRToken) -> bool, desc: S, loc: Loc,
) -> LazyResult<'err, IRToken> {
    match value {
        Some(tok) if pred(&tok) => Ok(tok),
        invalid => Err(invalid_option(invalid, desc, loc)),
    }
}

pub fn expect_token_by_option<'err, S: Display + 'err, T>(
    value: Option<IRToken>, pred: impl FnOnce(&IRToken) -> Option<T>, desc: S, loc: Loc,
) -> LazyResult<'err, Spanned<T>> {
    if let Some(tok) = value {
        if let Some(t) = pred(&tok) {
            return Ok((t, tok.loc()));
        }
    }

    Err(invalid_option(value, desc, loc))
}
