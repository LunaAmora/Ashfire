use firelib::lexer::Loc;

use super::parser::Parser;
use crate::compiler::{
    program::{Fmt, LazyError, LazyResult, Program, ProgramVisitor},
    types::{IRToken, KeywordType, Op, StructType},
};

impl Parser {
    pub fn expect_keyword(
        &mut self, key: KeywordType, error_text: &str, loc: Loc,
    ) -> LazyResult<IRToken> {
        self.expect_by(|tok| tok == key, error_text, loc)
    }

    pub fn expect_by(
        &mut self, pred: impl FnOnce(&IRToken) -> bool, error_text: &str, loc: Loc,
    ) -> LazyResult<IRToken> {
        expect_token_by(self.next(), pred, error_text, loc)
    }

    pub fn register_const(
        &mut self, assign: &KeywordType, struct_word: StructType, prog: &mut Program,
    ) {
        match assign {
            KeywordType::Colon => prog.consts.push(struct_word),
            KeywordType::Equal => self.register_var(struct_word, prog),
            _ => unreachable!(),
        }
    }

    pub fn register_var(&mut self, struct_word: StructType, prog: &mut Program) {
        match self.current_proc_mut(prog) {
            Some(proc) => proc.local_vars.push(struct_word),
            None => prog.global_vars.push(struct_word),
        }
    }
}

pub fn get_field_pos(vars: &[StructType], word: &str) -> Option<(usize, usize)> {
    let Some(i) = vars.iter().position(|v| v.name() == word) else {
        return  None;
    };

    let mut offset = 0;
    for (var, _) in vars.iter().zip(0..i) {
        offset += var.size() / 4;
    }

    Some((offset, i))
}

pub fn error_loc(error: &str, loc: Loc) -> LazyError {
    let error = error.to_string();
    LazyError::new(move |f| format!("{}{error}", f.format(Fmt::Loc(loc))))
}

pub fn invalid_option(tok: Option<IRToken>, desc: &str, loc: Loc) -> LazyError {
    let desc = desc.to_string();
    match tok {
        Some(tok) => LazyError::new(move |f| {
            format!(
                "{}Expected {desc}, but found: {} `{}`",
                f.format(Fmt::Loc(tok.loc)),
                f.format(Fmt::Typ(tok.token_type)),
                f.format(Fmt::Tok(tok))
            )
        }),
        None => LazyError::new(move |f| {
            format!("{}Expected {desc}, but found nothing", f.format(Fmt::Loc(loc)))
        }),
    }
}

pub fn invalid_token(tok: IRToken, error: &str) -> LazyError {
    let error = error.to_string();
    LazyError::new(move |f| {
        format!(
            "{}Invalid `{}` found on {error}: `{}`",
            f.format(Fmt::Loc(tok.loc)),
            f.format(Fmt::Typ(tok.token_type)),
            f.format(Fmt::Tok(tok))
        )
    })
}

pub fn format_block(error: &str, op: Op, loc: Loc) -> LazyError {
    let error = error.to_string();
    LazyError::new(move |f| {
        format!(
            concat!(
                "{}{}, but found a `{:?}` block instead\n",
                "[INFO] {}The found block started here."
            ),
            f.format(Fmt::Loc(loc)),
            error,
            op.op_type,
            f.format(Fmt::Loc(op.loc))
        )
    })
}

pub fn expect_token_by(
    value: Option<IRToken>, pred: impl FnOnce(&IRToken) -> bool, desc: &str, loc: Loc,
) -> LazyResult<IRToken> {
    match value {
        Some(tok) if pred(&tok) => Ok(tok),
        invalid => Err(invalid_option(invalid, desc, loc)),
    }
}

#[track_caller]
pub fn todo(loc: Loc) -> LazyResult<()> {
    Err(error_loc(&format!("\n[HERE]  {}", std::panic::Location::caller()), loc))
}