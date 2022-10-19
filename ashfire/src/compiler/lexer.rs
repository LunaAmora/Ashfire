use std::{
    fs::File,
    io::{BufRead, BufReader},
    ops::Not,
    path::PathBuf,
};

use anyhow::{Context, Result};
use ashlib::OptionErr;
use firelib::{bail, choice, ensure, ShortCircuit};

use super::{program::Program, types::*};

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
            Ok(read) if read > 0 => self.setup_buffer(line),
            _ => {
                self.buffer = Vec::new();
                false
            }
        }
    }

    fn setup_buffer(&mut self, line: String) -> bool {
        self.line_num += 1;
        (line.trim().is_empty() && self.read_line()) || self.setup_cursor_and_trim(line)
    }

    fn setup_cursor_and_trim(&mut self, line: String) -> bool {
        self.buffer = strip_trailing_newline(&line).chars().collect();
        self.col_num = 0;
        self.lex_pos = 0;
        self.seek_next_token()
    }

    fn seek_next_token(&mut self) -> bool {
        self.trim_left() || self.read_line()
    }

    fn read_from_pos(&self) -> String {
        self.buffer.iter().skip(self.lex_pos).collect()
    }

    fn trim_left(&mut self) -> bool {
        !self.buffer_done_or_empty() && self.advance_and_check_comments()
    }

    fn advance_and_check_comments(&mut self) -> bool {
        self.advance_by_predicate(|c| c != ' ');
        self.col_num = self.lex_pos;
        self.read_from_pos().starts_with("//").not()
    }

    fn buffer_done_or_empty(&self) -> bool {
        self.lex_pos + 1 > self.buffer.len() || self.read_from_pos().trim().is_empty()
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
            .collect()
    }

    fn next_token(&mut self) -> Option<Token> {
        self.seek_next_token()
            .then(|| Token::new(self.read_token(), self.current_loc()))
    }

    fn read_token(&mut self) -> String {
        self.read_by_predicate(|c| matches!(c, ' ' | ':'))
    }

    fn current_loc(&self) -> Loc {
        Loc::new(
            self.file.as_path().display().to_string(),
            self.line_num,
            self.col_num as i32 + 1,
        )
    }

    fn try_parse_string(&mut self, tok: &Token, program: &mut Program) -> OptionErr<IRToken> {
        let word = tok.name.strip_prefix('\"').or_return(OptionErr::default)?;

        let name = word
            .strip_suffix('\"')
            .map_or_else(|| self.read_string_literal(&tok.loc), |word| Ok(word.to_string()))?;

        OptionErr::new(program.define_string(name, tok))
    }

    fn read_string_literal(&mut self, loc: &Loc) -> Result<String> {
        self.advance_by_predicate(|c| c == '\"');
        Ok(self
            .read_by_predicate(|c| c == ' ')
            .strip_prefix('\"')
            .unwrap()
            .strip_suffix('\"')
            .with_context(|| format!("{loc}Missing closing `\"` in string literal"))?
            .to_string())
    }

    pub fn lex_next_token(&mut self, program: &mut Program) -> OptionErr<IRToken> {
        match self.next_token() {
            Some(tok) => choice!(
                OptionErr,
                self.try_parse_string(&tok, program),
                tok.parse_as_char(),
                tok.parse_as_keyword(),
                tok.parse_as_number(),
                OptionErr::new(program.define_word(tok))
            ),
            None => OptionErr::default(),
        }
    }
}

impl Program {
    fn define_string(&mut self, name: String, tok: &Token) -> IRToken {
        let index = self.push_data(&name, name.len() - escaped_len(&name)) as i32;
        IRToken::new(TokenType::Str, index, &tok.loc)
    }

    fn define_word(&mut self, tok: Token) -> IRToken {
        IRToken::new(TokenType::Word, self.add_word(tok.name), &tok.loc)
    }

    fn add_word(&mut self, name: String) -> i32 {
        self.words.push(name);
        self.words.len() as i32 - 1
    }
}

fn escaped_len(name: &str) -> usize {
    name.chars()
        .filter(|c| c.eq(&'\\'))
        .collect::<String>()
        .len()
}

fn strip_trailing_newline(input: &str) -> &str {
    input
        .strip_suffix("\r\n")
        .or_else(|| input.strip_suffix('\n'))
        .unwrap_or(input)
}

struct Token {
    name: String,
    loc: Loc,
}

impl Token {
    fn new(name: String, loc: Loc) -> Self {
        Self { name, loc }
    }

    fn parse_as_number(&self) -> Option<IRToken> {
        self.name
            .parse::<i32>()
            .ok()
            .map(|operand| IRToken::new(INT, operand, &self.loc))
    }

    fn parse_as_char(&self) -> OptionErr<IRToken> {
        match self.name.strip_prefix('\'') {
            Some(word) => match word.strip_suffix('\'') {
                Some(word) => parse_char(word, &self.loc),
                None => bail!("{}Missing closing `\'` in char literal", self.loc),
            }
            .value?
            .map(|operand| IRToken::new(INT, operand, &self.loc)),
            None => None,
        }
        .into()
    }

    fn parse_as_keyword(&self) -> Option<IRToken> {
        let operand = match self.name.as_str() {
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
        Some(IRToken::new(TokenType::Keyword, operand, &self.loc))
    }
}

fn parse_char(word: &str, loc: &Loc) -> OptionErr<i32> {
    match word.strip_prefix('\\') {
        Some(escaped) => parse_scaped(escaped, loc),
        None => {
            ensure!(
                word.len() == 1,
                "{loc}Char literals cannot contain more than one char: `{word}"
            );
            OptionErr::new(word.chars().next().unwrap() as i32)
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
