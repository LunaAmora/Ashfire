use std::{io::{BufRead, BufReader}, fs::File, path::PathBuf, ops::Not};
use super::{types::*, parser::Parser};
use anyhow::{Result, ensure, bail, Context};
use firelib::OptionErr;

struct Token {
    name: String,
    loc:  Loc
}

pub struct Lexer {
    buffer: Vec<char>,
    reader: BufReader<File>,
    file:   PathBuf,
    lex_pos:  usize,
    col_num:  usize,
    line_num: i32
}

impl Lexer {
    pub fn new(reader: BufReader<File>, file: PathBuf,) -> Self {
        Self {
            buffer: Vec::new(),
            reader,
            file,
            lex_pos: 0,
            col_num: 0,
            line_num: 0
        }
    }
    
    fn read_line(&mut self) -> bool {
        let mut line = String::new();
        match self.reader.read_line(&mut line) {
            Ok(read) if read > 0 => {
                self.line_num += 1;
    
                if line.trim().is_empty() {return self.read_line()}
    
                self.buffer = strip_trailing_newline(&line).chars().collect();
                self.col_num = 0;
                self.lex_pos = 0;

                if !self.trim_left() {return self.read_line()}
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
        }
        else {
            self.advance_by_predicate(|c: char| c != ' ');
            self.col_num = self.lex_pos;
            self.read_from_pos().starts_with("//").not()
        }
    }

    fn advance_by_predicate<T>(&mut self, func: T) where T: Fn(char) -> bool {
        while self.buffer.len() > self.lex_pos && !func(self.buffer[self.lex_pos]) {
            self.lex_pos += 1;
        } 
    }

    fn read_by_predicate<T>(&mut self, func: T) -> String where T: Fn(char) -> bool {
        self.advance_by_predicate(func);
        if self.col_num == self.lex_pos { self.lex_pos += 1}
        self.buffer.iter()
            .skip(self.col_num)
            .take(self.lex_pos - self.col_num)
            .collect::<String>()
    }

    fn next_token(&mut self) -> Option<Token> {
        if !self.trim_left() && !self.read_line() {return None}
        let pred = |c: char| c == ' ' || c == ':';
        let name = self.read_by_predicate(pred);
        let file = self.file.as_path().display().to_string();
        let loc = Loc { file, line: self.line_num, col: self.col_num as i32 + 1};
        Some(Token { name, loc })
    }

    fn try_parse_string(&mut self, token: &Token, parser: &mut Parser) -> Result<Option<IRToken>> {
        token.name
            .strip_prefix('\"').map_or_else(|| Ok(None), |word| {word
            .strip_suffix('\"').map_or_else(|| {
                self.advance_by_predicate(|c: char| c == '\"');
                Ok(self.read_by_predicate(|c: char| c == ' ')
                    .strip_prefix('\"').unwrap()
                    .strip_suffix('\"')
                    .with_context(|| format!("{} Missing closing `\"` in string literal", token.loc))?
                    .to_string())
            }, |word| Ok(word.to_string()))
            .map(|name| {
                let length = name.len() - name.chars().filter(|c| c.eq(&'\\')).collect::<String>().len();
                parser.data_list.push((Word { name, value: length as i32}).into());

                let operand = parser.data_list.len() as i32 - 1;
                Some(IRToken{ typ: TokenType::Str, operand, loc: token.loc.to_owned() })
            })
        })
    }

    pub fn lex_next_token(&mut self, parser: &mut Parser) -> Result<Option<IRToken>> {
        match self.next_token() {
            Some(tok) => { OptionErr::from(self
                .try_parse_string(&tok, parser))
                .or_try(|| try_parse_char(&tok))
                .or_else(|| parse_keyword(&tok))
                .or_else(|| parse_number(&tok))
                .or(|| parse_word(tok, parser))
            }
            None => Ok(None),
        }
    }
}

fn parse_word(tok: Token, parser: &mut Parser) -> IRToken {
    IRToken{ typ: TokenType::Word, operand: define_word(tok.name, parser), loc: tok.loc}
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
    tok.name.parse::<i32>().ok().map(|operand| 
        IRToken{ typ: TokenType::DataType(ValueType::Int), operand, loc: tok.loc.to_owned() })
}

fn try_parse_char(tok: &Token) -> Result<Option<IRToken>> {
    tok.name
        .strip_prefix('\'').map_or_else(|| Ok(None), |word| word
        .strip_suffix('\'').with_context(|| format!("{} Missing closing `\'` in char literal", tok.loc))?
        .strip_prefix('\\').map_or_else(|| {
            ensure!(word.len() == 2, "{} Char literals cannot contain more than one char: `{word}", tok.loc);
            Ok(Some(word.chars().next().unwrap() as i32))
        }, |escaped| Ok(
            match escaped {
                "t" => Some('\t' as i32),
                "n" => Some('\n' as i32),
                "r" => Some('\r' as i32),
                "\'" => Some('\'' as i32),
                "\\" => Some('\\' as i32),
                _ if escaped.len() == 2 => try_parse_hex(escaped),
                _ => bail!("{} Invalid escaped character sequence found on char literal: `{escaped}`", tok.loc),
            }
        ))
        .map(|o| o.map(|operand|
            IRToken{ typ: TokenType::DataType(ValueType::Int), operand, loc: tok.loc.to_owned() }
        ))
    )
}

fn try_parse_hex(word: &str) ->  Option<i32> {
    i64::from_str_radix(word, 16).ok().map(|h| h as i32)
}

fn parse_keyword(tok: &Token) -> Option<IRToken> {
    let operand = match tok.name.as_str() {
        "dup"  => KeywordType::Dup,
        "swap" => KeywordType::Swap,
        "drop" => KeywordType::Drop,
        "over" => KeywordType::Over,
        "rot"  => KeywordType::Rot,
        "if"   => KeywordType::If,
        "else" => KeywordType::Else,
        "end"  => KeywordType::End,
        "proc" => KeywordType::Proc,
        "->"   => KeywordType::Arrow,
        "mem"  => KeywordType::Mem,
        ":"    => KeywordType::Colon,
        "="    => KeywordType::Equal,
        "let"  => KeywordType::Let,
        "do"   => KeywordType::Do,
        "@"    => KeywordType::At,
        "case" => KeywordType::Case,
        "while"  => KeywordType::While,
        "struct" => KeywordType::Struct,
        "include" => KeywordType::Include,
        _ => return None
    }.into();
    Some(IRToken{ typ: TokenType::Keyword, operand, loc: tok.loc.to_owned() })
}
