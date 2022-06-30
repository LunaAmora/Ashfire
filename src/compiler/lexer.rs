use std::{io::{BufRead, BufReader}, fs::File, path::PathBuf, ops::Not};
use super::{types::*, parser::Parser};
use anyhow::{Result, ensure, bail};

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

    fn try_parse_string(&mut self, token: &Token, parser: &mut Parser) -> Result<Option<i32>> {
        let mut name = token.name.clone();
        
        if !name.starts_with('\"') { return Ok(None) }
        if !name.ends_with('\"') {
            self.advance_by_predicate(|c: char| c == '\"');
            name = self.read_by_predicate(|c: char| c == ' ');
            ensure!(&name.ends_with('\"'), "Missing closing `\"` in string literal");
        }

        name = name.trim_matches('\"').to_string();
        
        let length = name.len() - name.chars().filter(|c| c.eq(&'\\')).collect::<String>().len();
        let word = Word { name, value: length as i32};
        parser.data_list.push(word.into());

        Ok(Some(parser.data_list.len() as i32 - 1))
    }

    pub fn lex_next_token(&mut self, parser: &mut Parser) -> Result<Option<IRToken>> {
        Ok(match self.next_token() {
            Some(tok) => { Some(
                if let Some(operand) = self.try_parse_string(&tok, parser)? {
                    IRToken{ typ: TokenType::Str, operand, loc: tok.loc }
                } else if let Some(operand) = try_parse_char(&tok.name)? {
                    IRToken{ typ: TokenType::DataType(0), operand, loc: tok.loc }
                } else if let Some(operand) = try_parse_keyword(&tok.name) {
                    IRToken{ typ: TokenType::Keyword, operand, loc: tok.loc }
                } else if let Some(operand) = try_parse_number(&tok.name) {
                    IRToken{ typ: TokenType::DataType(0), operand, loc: tok.loc }
                } else {
                    IRToken{ typ: TokenType::Word, operand: define_word(tok.name, parser), loc: tok.loc}
                })
            }
            None => None,
        })
    }
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

fn try_parse_number(word: &str) ->  Option<i32> {
    word.parse::<i32>().ok()
}

fn try_parse_char(word: &str) -> Result<Option<i32>> {
    if let Some(word) = word.strip_prefix('\'') {
        if let Some(word) = word.strip_suffix('\'') { return Ok(
            if let Some(escaped) = word.strip_prefix('\\') {
                match escaped {
                    "t" => Some('\t' as i32),
                    "n" => Some('\n' as i32),
                    "r" => Some('\r' as i32),
                    "\'" => Some('\'' as i32),
                    "\\" => Some('\\' as i32),
                    _ if escaped.len() == 2 => try_parse_hex(escaped),
                    _ => bail!("Invalid escaped character sequence found on char literal: `{}`", escaped),
                }
            } else {
                ensure!(word.len() == 1, "Char literals cannot contain more than one char: `{}`", word);
                Some(word.chars().next().unwrap() as i32)
            })
        }
        bail!("Missing closing `\'` in char literal")
    }
    Ok(None)
}

fn try_parse_hex(word: &str) ->  Option<i32> {
    i64::from_str_radix(word, 16).ok().map(|h| h as i32)
}

fn try_parse_keyword(word: &str) -> Option<i32> { Some(
    match word {
        "dup"  => KeywordType::Dup.into(),
        "swap" => KeywordType::Swap.into(),
        "drop" => KeywordType::Drop.into(),
        "over" => KeywordType::Over.into(),
        "rot"  => KeywordType::Rot.into(),
        "if"   => KeywordType::If.into(),
        "else" => KeywordType::Else.into(),
        "end"  => KeywordType::End.into(),
        "proc" => KeywordType::Proc.into(),
        "->"   => KeywordType::Arrow.into(),
        "mem"  => KeywordType::Mem.into(),
        ":"    => KeywordType::Colon.into(),
        "="    => KeywordType::Equal.into(),
        "let"  => KeywordType::Let.into(),
        "do"   => KeywordType::Do.into(),
        "@"    => KeywordType::At.into(),
        "case" => KeywordType::Case.into(),
        "while"  => KeywordType::While.into(),
        "struct" => KeywordType::Struct.into(),
        "include" => KeywordType::Include.into(),
        _ => return None
    })
}
