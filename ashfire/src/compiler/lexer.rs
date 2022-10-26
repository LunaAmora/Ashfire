use std::path::PathBuf;

use ashlib::OptionErr;
use firelib::{
    anyhow::{Context, Result},
    lexer::*,
    ShortCircuit,
};

use super::{program::Program, types::*};

impl Program {
    pub fn new_lexer(file: &PathBuf) -> Result<Lexer> {
        Lexer::builder(file)
            .with_separators(vec![':', '='])
            .with_matches(vec![Match::Same('\''), Match::Same('\"'), Match::Pair('(', ')')])
            .with_comments("//")
            .build()
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
        tok.name
            .strip_prefix('\"')
            .or_return(OptionErr::default)?
            .strip_suffix('\"')
            .with_context(|| format!("{}Missing closing `\"` in string literal", tok.loc))
            .map(|name| self.push_data(name, name.len() - escaped_len(&name)) as i32)
            .map(|operand| IRToken::new(TokenType::Str, operand, &tok.loc))
            .map(OptionErr::new)?
    }

    fn define_word(&mut self, tok: Token) -> OptionErr<IRToken> {
        self.words.push(tok.name);
        let operand = self.words.len() as i32 - 1;

        OptionErr::new(IRToken::new(TokenType::Word, operand, &tok.loc))
    }
}

fn escaped_len(name: &str) -> usize {
    name.chars()
        .filter(|&c| c == '\\')
        .collect::<String>()
        .len()
}

fn parse_as_char(tok: &Token) -> OptionErr<IRToken> {
    tok.name
        .strip_prefix('\'')
        .or_return(OptionErr::default)?
        .strip_suffix('\'')
        .with_context(|| format!("{}Missing closing `\'` in char literal", tok.loc))
        .and_then(|word| parse_char(word, &tok.loc).value)?
        .map(|operand| IRToken::new(INT, operand, &tok.loc))
        .into()
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
        "case" => KeywordType::Case,
        "while" => KeywordType::While,
        "struct" => KeywordType::Struct,
        "include" => KeywordType::Include,
        _ => return None,
    } as i32;
    Some(IRToken::new(TokenType::Keyword, operand, &tok.loc))
}

fn parse_as_number(tok: &Token) -> Option<IRToken> {
    tok.name
        .parse::<i32>()
        .ok()
        .map(|operand| IRToken::new(INT, operand, &tok.loc))
}

fn parse_char(word: &str, loc: &Loc) -> OptionErr<i32> {
    match word.strip_prefix('\\') {
        Some(escaped) => parse_scaped(escaped, loc),
        None => match word.len() {
            0 => bail!("{loc}Char literals have to contain at leat on char"),
            2.. => bail!("{loc}Char literals cannot contain more than one char: `{word}`"),
            _ => OptionErr::new(word.chars().next().unwrap() as i32),
        },
    }
}

fn parse_scaped(escaped: &str, loc: &Loc) -> OptionErr<i32> {
    Some(match escaped {
        "t" => '\t' as i32,
        "n" => '\n' as i32,
        "r" => '\r' as i32,
        "\'" => '\'' as i32,
        "\\" => '\\' as i32,
        _ if escaped.len() == 2 => try_parse_hex(escaped).with_context(|| {
            format!("{loc}Invalid characters found on char literal: `\\{escaped}`")
        })?,
        _ => bail!("{loc}Invalid escaped character sequence found on char literal: `{escaped}`"),
    })
    .into()
}

fn try_parse_hex(word: &str) -> Option<i32> {
    i64::from_str_radix(word, 16).ok().map(|h| h as i32)
}
