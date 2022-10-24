use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

use anyhow::{Context, Result};
use ashlib::OptionErr;
use firelib::{bail, choice, ShortCircuit};

use super::{program::Program, types::*};

#[derive(Default)]
pub struct LexerBuilder {
    file: Option<PathBuf>,
    separators: Vec<char>,
    comments: Option<String>,
}

impl LexerBuilder {
    pub fn new() -> Self {
        Self { ..Default::default() }
    }

    pub fn with_path(mut self, file: &PathBuf) -> Self {
        self.file = Some(file.to_owned());
        self
    }

    pub fn with_separators(mut self, sep: Vec<char>) -> Self {
        self.separators.extend(sep);
        self
    }

    pub fn with_comments(mut self, comment: &str) -> Self {
        self.comments = Some(comment.to_owned());
        self
    }

    pub fn build(self) -> Result<Lexer> {
        let filepath = self
            .file
            .with_context(|| "Missing file path to build the lexer")?;

        let file = File::open(&filepath)
            .with_context(|| format!("Could not read file `{:?}`", filepath))?;

        let reader = BufReader::new(file);

        Ok(Lexer::new(reader, filepath, self.separators, self.comments))
    }
}

enum Predicate {
    Whitespace,
    Separators,
}

pub struct Lexer {
    buffer: Vec<char>,
    reader: BufReader<File>,
    separators: Vec<char>,
    comments: Option<String>,
    file: PathBuf,
    lex_pos: usize,
    col_num: usize,
    line_num: i32,
}

impl Lexer {
    pub fn new(
        reader: BufReader<File>, file: PathBuf, separators: Vec<char>, comments: Option<String>,
    ) -> Self {
        Self {
            reader,
            file,
            separators,
            comments,
            buffer: Vec::new(),
            lex_pos: 0,
            col_num: 0,
            line_num: 0,
        }
    }

    pub fn builder() -> LexerBuilder {
        LexerBuilder::new()
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
        !self.buffer_done_or_empty() && self.check_for_comments()
    }

    fn check_for_comments(&mut self) -> bool {
        self.advance_by_predicate(Predicate::Whitespace);
        self.col_num = self.lex_pos;
        self.comments
            .as_ref()
            .map_or(false, |pattern| !self.read_from_pos().starts_with(pattern))
    }

    fn buffer_done_or_empty(&self) -> bool {
        self.lex_pos + 1 > self.buffer.len() || self.read_from_pos().trim().is_empty()
    }

    fn advance_by_predicate(&mut self, pred: Predicate) {
        let &start = self.buffer.get(self.col_num).unwrap();

        while let Some(&buf) = self.buffer.get(self.lex_pos) {
            if match pred {
                Predicate::Separators if self.col_num == self.lex_pos => false,
                Predicate::Separators if matches!(start, '\'' | '\"') => {
                    if matches!(buf, '\'' | '\"') {
                        self.lex_pos += 1;
                        true
                    } else {
                        false
                    }
                }
                Predicate::Separators => buf == ' ' || self.separators.contains(&buf),
                Predicate::Whitespace => buf != ' ',
            } {
                break;
            }

            self.lex_pos += 1;
        }
    }

    fn read_by_predicate(&mut self, pred: Predicate) -> String {
        self.advance_by_predicate(pred);

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
        self.read_by_predicate(Predicate::Separators)
    }

    fn current_loc(&self) -> Loc {
        Loc::new(
            self.file.as_path().display().to_string(),
            self.line_num,
            self.col_num as i32 + 1,
        )
    }

    fn parse_as_string<F>(&mut self, tok: &Token, mut define_string: F) -> OptionErr<IRToken>
    where
        F: FnMut(&str, &Token) -> IRToken,
    {
        let name = tok
            .name
            .strip_prefix('\"')
            .or_return(OptionErr::default)?
            .strip_suffix('\"')
            .with_context(|| format!("{}Missing closing `\"` in string literal", tok.loc))?;

        OptionErr::new(define_string(name, tok))
    }

    pub fn lex_next_token(&mut self, program: &mut Program) -> OptionErr<IRToken> {
        let tok = self.next_token().or_return(OptionErr::default)?;

        choice!(
            OptionErr,
            self.parse_as_string(&tok, |name, tok| program.define_string(name, tok)),
            tok.parse_as_char(),
            tok.parse_as_keyword(),
            tok.parse_as_number(),
            tok.define_as_word(|tok| program.define_word(tok)),
        )
    }
}

impl Program {
    fn define_string(&mut self, name: &str, tok: &Token) -> IRToken {
        let index = self.push_data(name, name.len() - escaped_len(&name)) as i32;
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
        .filter(|&c| c == '\\')
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
        self.name
            .strip_prefix('\'')
            .or_return(OptionErr::default)?
            .strip_suffix('\'')
            .with_context(|| format!("{}Missing closing `\'` in char literal", self.loc))
            .and_then(|word| parse_char(word, &self.loc).value)?
            .map(|operand| IRToken::new(INT, operand, &self.loc))
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

    fn define_as_word<F>(self, mut define_word: F) -> OptionErr<IRToken>
    where
        F: FnMut(Token) -> IRToken,
    {
        OptionErr::new(define_word(self))
    }
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
