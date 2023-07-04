use std::io::Read;

use ashfire_types::{
    core::{IRToken, IRTokenExt, TokenType},
    data::TypeId,
};
use firelib::{lazy::LazyCtx, lexer::*, ShortCircuit};

use super::{
    ctx::{Ctx, Fmt, OptionErr},
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

fn new_lexer<'r>(buf_id: usize, reader: impl Read + 'r) -> Lexer<'r> {
    builder().build(buf_id, reader)
}

impl Ctx {
    pub fn new_lexer<'r>(
        &mut self, reader: impl Read + 'r, source: &str, module: &str,
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

    fn parse_as_string(&mut self, &(ref name, loc): &Token) -> OptionErr<IRToken> {
        name.strip_prefix('\"')
            .or_return(OptionErr::default)?
            .strip_suffix('\"')
            .with_err_ctx(move || err_loc("Missing closing `\"` in string literal", loc))
            .map(|string| self.push_data(string.to_owned(), escaped_len(string)))
            .map(|operand| (TokenType::Str(operand), loc))
            .into()
    }

    fn define_word(&self, &(ref name, loc): &Token) -> OptionErr<IRToken> {
        let name = self.get_or_intern(name);
        OptionErr::new((TokenType::Word(name), loc))
    }
}

fn parse_as_char(&(ref name, loc): &Token) -> OptionErr<IRToken> {
    let word = name
        .strip_prefix('\'')
        .or_return(OptionErr::default)?
        .strip_suffix('\'')
        .with_err_ctx(move || err_loc("Missing closing `\'` in char literal", loc))?;

    parse_char(word, loc)
        .value?
        .map(|operand| IRToken::data(TypeId::INT, operand, loc))
        .into()
}

fn parse_char(word: &str, loc: Loc) -> OptionErr<i32> {
    word.strip_prefix('\\').map_or_else(
        || match word.len() {
            0 => err_loc("Char literals have to contain at least one char", loc).into(),

            2.. => {
                err_loc(format!("Char literals cannot contain more than one char: `{word}`"), loc)
                    .into()
            }

            _ => word
                .chars()
                .next()
                .map(|char| u8::try_from(char).expect("ICE").into())
                .into(),
        },
        |escaped| parse_scaped(escaped.to_owned(), loc),
    )
}

fn parse_scaped(escaped: String, loc: Loc) -> OptionErr<i32> {
    let char = match escaped.as_str() {
        "t" => '\t',
        "n" => '\n',
        "r" => '\r',
        "\'" => '\'',
        "\\" => '\\',

        _ if escaped.len() == 2 => {
            return try_parse_hex(&escaped)
                .with_err_ctx(move || {
                    err_loc(format!("Invalid characters found on char literal: `\\{escaped}`"), loc)
                })
                .into();
        }

        _ => lazybail!(
            |f| "{}Invalid escaped character sequence found on char literal: `{escaped}`",
            f(Fmt::Loc(loc))
        ),
    };

    OptionErr::new(u8::try_from(char).expect("ICE").into())
}

fn escaped_len(name: &str) -> u16 {
    name.chars()
        .filter(|&c| c != '\\')
        .count()
        .try_into()
        .expect("ICE")
}

fn parse_as_keyword((name, loc): &Token) -> Option<IRToken> {
    name.parse().map(|k| (TokenType::Keyword(k), *loc)).ok()
}

fn parse_as_number((name, loc): &Token) -> Option<IRToken> {
    name.parse()
        .map(|operand| IRToken::data(TypeId::INT, operand, *loc))
        .ok()
}

fn try_parse_hex(word: &str) -> Option<i32> {
    i32::from_str_radix(word, 16).ok()
}
