use std::{fs::File, path::PathBuf};

use firelib::{
    anyhow::{Context, Result},
    lazy::LazyCtx,
    lexer::*,
    ShortCircuit,
};

use super::{
    program::{Fmt, OptionErr, Program},
    types::*,
};

impl Program {
    pub fn new_lexer(&mut self, path: &PathBuf) -> Result<Lexer> {
        let file = File::open(path).with_context(|| format!("Could not read file `{:?}`", path))?;

        self.included_files.push(path.to_str().unwrap().to_owned());

        Ok(Lexer::builder(file)
            .with_separators(vec![':', '=', '*'])
            .with_matches(vec![
                Match::Same('\''),
                Match::Same('\"'),
                Match::Pair('(', ')'),
                Match::Pair('#', ' '),
            ])
            .with_comments("//")
            .build(self.included_files.len() - 1))
    }

    pub fn lex_next_token(&mut self, lexer: &mut Lexer) -> OptionErr<IRToken> {
        let tok = lexer.next().or_return(OptionErr::default)?;

        choice!(
            OptionErr,
            self.parse_as_string(&tok),
            parse_as_char(&tok),
            parse_as_keyword(&tok),
            parse_as_number(&tok),
            self.define_word(tok),
        )
    }

    fn parse_as_string(&mut self, tok: &Token) -> OptionErr<IRToken> {
        let loc = tok.loc;
        tok.name
            .strip_prefix('\"')
            .or_return(OptionErr::default)?
            .strip_suffix('\"')
            .with_ctx(move |f| {
                format!("{}Missing closing `\"` in string literal", f.format(Fmt::Loc(loc)))
            })
            .map(|name| self.push_data(name, name.len() - escaped_len(&name)) as i32)
            .map(|operand| IRToken::new(TokenType::Str, operand, loc))
            .map(OptionErr::new)?
    }

    fn define_word(&mut self, tok: Token) -> OptionErr<IRToken> {
        self.words.push(tok.name);
        let operand = self.words.len() as i32 - 1;

        OptionErr::new(IRToken::new(TokenType::Word, operand, tok.loc))
    }
}

fn parse_as_char(tok: &Token) -> OptionErr<IRToken> {
    let loc = tok.loc;
    let word = tok
        .name
        .strip_prefix('\'')
        .or_return(OptionErr::default)?
        .strip_suffix('\'')
        .with_ctx(move |f| {
            format!("{}Missing closing `\'` in char literal", f.format(Fmt::Loc(loc)))
        })?;

    parse_char(word.to_string(), tok.loc)
        .value?
        .map(|operand| IRToken::new(INT, operand, tok.loc))
        .into()
}

fn parse_char(word: String, loc: Loc) -> OptionErr<i32> {
    match word.strip_prefix('\\') {
        Some(escaped) => parse_scaped(escaped.to_owned(), loc),
        None => match word.len() {
            0 => lazybail!(
                |f| "{}Char literals have to contain at leat on char",
                f.format(Fmt::Loc(loc))
            ),
            2.. => lazybail!(
                |f| "{}Char literals cannot contain more than one char: `{word}`",
                f.format(Fmt::Loc(loc))
            ),
            _ => OptionErr::new(word.chars().next().unwrap() as i32),
        },
    }
}

fn parse_scaped(escaped: String, loc: Loc) -> OptionErr<i32> {
    Some(match escaped.as_str() {
        "t" => '\t' as i32,
        "n" => '\n' as i32,
        "r" => '\r' as i32,
        "\'" => '\'' as i32,
        "\\" => '\\' as i32,
        _ if escaped.len() == 2 => try_parse_hex(&escaped).with_ctx(move |f| {
            format!(
                "{}Invalid characters found on char literal: `\\{escaped}`",
                f.format(Fmt::Loc(loc))
            )
        })?,
        _ => lazybail!(
            |f| "{}Invalid escaped character sequence found on char literal: `{escaped}`",
            f.format(Fmt::Loc(loc))
        ),
    })
    .into()
}

fn escaped_len(name: &str) -> usize {
    name.chars()
        .filter(|&c| c == '\\')
        .collect::<String>()
        .len()
}

fn parse_as_keyword(tok: &Token) -> Option<IRToken> {
    let operand = match tok.name.as_str() {
        "dup" => KeywordType::Dup,
        "swap" => KeywordType::Swap,
        "drop" => KeywordType::Drop,
        "over" => KeywordType::Over,
        "rot" => KeywordType::Rot,
        "if" => KeywordType::If,
        "else" => KeywordType::Else,
        "end" => KeywordType::End,
        "proc" => KeywordType::Proc,
        "->" => KeywordType::Arrow,
        "mem" => KeywordType::Mem,
        ":" => KeywordType::Colon,
        "=" => KeywordType::Equal,
        "let" => KeywordType::Let,
        "do" => KeywordType::Do,
        "@" => KeywordType::At,
        "." => KeywordType::Dot,
        "*" => KeywordType::Ref,
        "case" => KeywordType::Case,
        "while" => KeywordType::While,
        "struct" => KeywordType::Struct,
        "inline" => KeywordType::Inline,
        "include" => KeywordType::Include,
        _ => return None,
    } as i32;
    Some(IRToken::new(TokenType::Keyword, operand, tok.loc))
}

fn parse_as_number(tok: &Token) -> Option<IRToken> {
    tok.name
        .parse::<i32>()
        .ok()
        .map(|operand| IRToken::new(INT, operand, tok.loc))
}

fn try_parse_hex(word: &str) -> Option<i32> {
    i64::from_str_radix(word, 16).ok().map(|h| h as i32)
}
