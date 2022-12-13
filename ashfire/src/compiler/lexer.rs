use std::{io::BufRead, str::FromStr};

use ashfire_types::{
    core::{IRToken, TokenType, INT},
    enums::KeywordType,
    lasso::Key,
};
use firelib::{lazy::LazyCtx, lexer::*, ShortCircuit};

use super::{
    program::{Fmt, OptionErr, Program},
    utils::err_loc,
};

fn builder() -> LexerBuilder {
    Lexer::builder()
        .with_separators([':', '=', '*'])
        .with_matches([
            Match::Same('\''),
            Match::Same('\"'),
            Match::Pair('(', ')'),
            Match::Pair('#', ' '),
        ])
        .with_comments("//")
}

fn new_lexer(buf_id: usize, reader: &mut impl BufRead) -> Lexer {
    builder().build(buf_id, reader)
}

impl Program {
    pub fn new_lexer<'r>(
        &mut self, reader: &'r mut impl BufRead, source: &str, module: &str,
    ) -> Lexer<'r> {
        new_lexer(self.push_source(source, module), reader)
    }

    pub fn lex_next_token(&mut self, lexer: &mut Lexer) -> OptionErr<IRToken> {
        let tok = lexer.next().or_return(OptionErr::default)?;

        choice!(
            OptionErr,
            self.parse_as_string(&tok),
            parse_as_char(&tok),
            parse_as_keyword(&tok),
            parse_as_number(&tok),
            self.define_word(&tok),
        )
    }

    fn parse_as_string(&mut self, tok: &Token) -> OptionErr<IRToken> {
        let loc = tok.loc;
        tok.name
            .strip_prefix('\"')
            .or_return(OptionErr::default)?
            .strip_suffix('\"')
            .with_err_ctx(move || err_loc("Missing closing `\"` in string literal", loc))
            .map(|name| self.push_data(name.to_owned(), escaped_len(name)))
            .map(|operand| IRToken(TokenType::Str, operand as i32, loc))
            .map(OptionErr::new)?
    }

    fn define_word(&mut self, tok: &Token) -> OptionErr<IRToken> {
        let operand = self.get_or_intern(&tok.name).into_usize();
        OptionErr::new(IRToken(TokenType::Word, operand as i32, tok.loc))
    }
}

fn parse_as_char(tok: &Token) -> OptionErr<IRToken> {
    let loc = tok.loc;
    let word = tok
        .name
        .strip_prefix('\'')
        .or_return(OptionErr::default)?
        .strip_suffix('\'')
        .with_err_ctx(move || err_loc("Missing closing `\'` in char literal", loc))?;

    parse_char(word, loc)
        .value?
        .map(|operand| IRToken(INT, operand, loc))
        .into()
}

fn parse_char(word: &str, loc: Loc) -> OptionErr<i32> {
    word.strip_prefix('\\').map_or_else(
        || match word.len() {
            0 => err_loc("Char literals have to contain at leat on char", loc).into(),
            2.. => {
                err_loc(format!("Char literals cannot contain more than one char: `{word}`"), loc)
                    .into()
            }
            _ => OptionErr::new(word.chars().next().unwrap() as i32),
        },
        |escaped| parse_scaped(escaped.to_owned(), loc),
    )
}

fn parse_scaped(escaped: String, loc: Loc) -> OptionErr<i32> {
    OptionErr::new(match escaped.as_str() {
        "t" => '\t' as i32,
        "n" => '\n' as i32,
        "r" => '\r' as i32,
        "\'" => '\'' as i32,
        "\\" => '\\' as i32,
        _ if escaped.len() == 2 => try_parse_hex(&escaped).with_err_ctx(move || {
            err_loc(format!("Invalid characters found on char literal: `\\{escaped}`"), loc)
        })?,
        _ => lazybail!(
            |f| "{}Invalid escaped character sequence found on char literal: `{escaped}`",
            f.format(Fmt::Loc(loc))
        ),
    })
}

fn escaped_len(name: &str) -> usize {
    name.chars().filter(|&c| c != '\\').count()
}

fn parse_as_keyword(tok: &Token) -> Option<IRToken> {
    KeywordType::from_str(tok.name.as_str())
        .ok()
        .map(|k| IRToken(TokenType::Keyword, k as i32, tok.loc))
}

fn parse_as_number(tok: &Token) -> Option<IRToken> {
    tok.name
        .parse::<i32>()
        .ok()
        .map(|operand| IRToken(INT, operand, tok.loc))
}

fn try_parse_hex(word: &str) -> Option<i32> {
    i64::from_str_radix(word, 16).ok().map(|h| h as i32)
}
