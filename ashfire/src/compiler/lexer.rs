use super::{parser::Parser, types::*};
use anyhow::{Context, Result};
use firelib::*;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    ops::Not,
    path::PathBuf,
};

struct Token {
    name: String,
    loc: Loc,
}

pub struct Lexer {
    buffer: Vec<char>,
    reader: BufReader<File>,
    file: PathBuf,
    lex_pos: usize,
    col_num: usize,
    line_num: i32,
}

impl Lexer {
    pub fn new(reader: BufReader<File>, file: &PathBuf) -> Self {
        Self {
            buffer: Vec::new(),
            reader,
            file: file.to_owned(),
            lex_pos: 0,
            col_num: 0,
            line_num: 0,
        }
    }

    fn read_line(&mut self) -> bool {
        let mut line = String::new();
        match self.reader.read_line(&mut line) {
            Ok(read) if read > 0 => {
                self.line_num += 1;

                if line.trim().is_empty() {
                    return self.read_line();
                }

                self.buffer = strip_trailing_newline(&line).chars().collect();
                self.col_num = 0;
                self.lex_pos = 0;

                if !self.trim_left() {
                    return self.read_line();
                }
                true
            }
            _ => {
                self.buffer = Vec::new();
                false
            }
        }
    }

    fn read_from_pos(&mut self) -> String {
        self.buffer.iter().skip(self.lex_pos).collect()
    }

    fn trim_left(&mut self) -> bool {
        if self.lex_pos + 1 > self.buffer.len() || self.read_from_pos().trim().is_empty() {
            false
        } else {
            self.advance_by_predicate(|c: char| c != ' ');
            self.col_num = self.lex_pos;
            self.read_from_pos().starts_with("//").not()
        }
    }

    fn advance_by_predicate(&mut self, func: impl Fn(char) -> bool) {
        while self.buffer.len() > self.lex_pos && !func(self.buffer[self.lex_pos]) {
            self.lex_pos += 1;
        }
    }

    fn read_by_predicate(&mut self, func: impl Fn(char) -> bool) -> String {
        self.advance_by_predicate(func);
        if self.col_num == self.lex_pos {
            self.lex_pos += 1
        }
        self.buffer
            .iter()
            .skip(self.col_num)
            .take(self.lex_pos - self.col_num)
            .collect::<String>()
    }

    fn next_token(&mut self) -> Option<Token> {
        if !self.trim_left() && !self.read_line() {
            return None;
        }
        let pred = |c: char| matches!(c, ' ' | ':');
        let name = self.read_by_predicate(pred);
        let file = self.file.as_path().display().to_string();
        let col = self.col_num as i32 + 1;
        let loc = Loc { file, line: self.line_num, col };
        Some(Token { name, loc })
    }

    fn try_parse_string(&mut self, tok: &Token, parser: &mut Parser) -> OptionErr<IRToken> {
        tok.name
            .strip_prefix('\"')
            .map_or_else(OptionErr::default, |word| {
                word.strip_suffix('\"')
                    .map_or_else(|| self.read_string_literal(&tok.loc), |word| Ok(word.to_string()))
                    .map(|name| {
                        (
                            Word::new(&name, (name.len() - scapped_len(&name)) as i32),
                            parser.data_list.len() as i32,
                        )
                    })
                    .map(|(word, operand)| {
                        parser.data_list.push(word.into());
                        Some(IRToken::new(TokenType::Str, operand, &tok.loc))
                    })
                    .into()
            })
    }

    fn read_string_literal(&mut self, loc: &Loc) -> Result<String> {
        self.advance_by_predicate(|c| c == '\"');
        Ok(self
            .read_by_predicate(|c| c == ' ')
            .strip_prefix('\"')
            .expect("unreachable")
            .strip_suffix('\"')
            .with_context(|| format!("{loc}Missing closing `\"` in string literal"))?
            .to_string())
    }

    pub fn lex_next_token(&mut self, parser: &mut Parser) -> OptionErr<IRToken> {
        match self.next_token() {
            Some(tok) => choice!(
                OptionErr,
                self.try_parse_string(&tok, parser),
                try_parse_char(&tok),
                parse_keyword(&tok),
                parse_number(&tok),
                parse_word(tok, parser)
            ),
            None => OptionErr::default(),
        }
    }
}

fn scapped_len(name: &str) -> usize {
    name.chars()
        .filter(|c| c.eq(&'\\'))
        .collect::<String>()
        .len()
}

fn parse_word(tok: Token, parser: &mut Parser) -> IRToken {
    IRToken::new(TokenType::Word, define_word(tok.name, parser), &tok.loc)
}

fn define_word(name: String, parser: &mut Parser) -> i32 {
    parser.word_list.push(name);
    parser.word_list.len() as i32 - 1
}

fn strip_trailing_newline(input: &str) -> &str {
    input
        .strip_suffix("\r\n")
        .or_else(|| input.strip_suffix('\n'))
        .unwrap_or(input)
}

fn parse_number(tok: &Token) -> Option<IRToken> {
    tok.name
        .parse::<i32>()
        .ok()
        .map(|operand| IRToken::new(ValueType::Int.into(), operand, &tok.loc))
}

fn try_parse_char(tok: &Token) -> OptionErr<IRToken> {
    let loc = &tok.loc;
    match tok.name.strip_prefix('\'') {
        Some(word) => match word.strip_suffix('\'') {
            Some(word) => parse_char(word, loc),
            None => bail!("{loc}Missing closing `\'` in char literal"),
        }
        .value?
        .map(|operand| (operand, loc.clone()).into()),
        None => None,
    }
    .into()
}

fn parse_char(word: &str, loc: &Loc) -> OptionErr<i32> {
    match word.strip_prefix('\\') {
        Some(escaped) => parse_scaped(escaped, loc),
        None => {
            ensure!(
                word.len() == 1,
                "{loc}Char literals cannot contain more than one char: `{word}"
            );
            (word.chars().next().unwrap() as i32).into()
        }
    }
}

fn parse_scaped(escaped: &str, loc: &Loc) -> OptionErr<i32> {
    match escaped {
        "t" => Some('\t' as i32),
        "n" => Some('\n' as i32),
        "r" => Some('\r' as i32),
        "\'" => Some('\'' as i32),
        "\\" => Some('\\' as i32),
        _ if escaped.len() == 2 => try_parse_hex(escaped),
        _ => bail!("{loc}Invalid escaped character sequence found on char literal: `{escaped}`"),
    }
    .into()
}

fn try_parse_hex(word: &str) -> Option<i32> {
    i64::from_str_radix(word, 16).ok().map(|h| h as i32)
}

fn parse_keyword(tok: &Token) -> Option<IRToken> {
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
    }
    .into();
    Some(IRToken::new(TokenType::Keyword, operand, &tok.loc))
}
