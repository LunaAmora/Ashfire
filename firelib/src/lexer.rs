use core::fmt;
use std::{
    fmt::{Display, Formatter},
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

use anyhow::{Context, Result};

use crate::utils::strip_trailing_newline;

#[derive(Default)]
pub struct LexerBuilder {
    file: PathBuf,
    separators: Vec<char>,
    comments: Option<String>,
}

impl LexerBuilder {
    fn new(file: PathBuf) -> Self {
        Self { file, ..Default::default() }
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
        let file = File::open(&self.file)
            .with_context(|| format!("Could not read file `{:?}`", self.file))?;

        let reader = BufReader::new(file);
        Ok(Lexer::new(reader, self.file, self.separators, self.comments))
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

    pub fn builder(file: &PathBuf) -> LexerBuilder {
        LexerBuilder::new(file.to_owned())
    }

    pub fn next_token(&mut self) -> Option<Token> {
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

    fn seek_next_token(&mut self) -> bool {
        self.trim_left() || self.read_line()
    }

    fn trim_left(&mut self) -> bool {
        !self.buffer_done_or_empty() && self.check_for_comments()
    }

    fn buffer_done_or_empty(&self) -> bool {
        self.lex_pos + 1 > self.buffer.len() || self.read_from_pos().trim().is_empty()
    }

    fn check_for_comments(&mut self) -> bool {
        self.advance_by_predicate(Predicate::Whitespace);
        self.col_num = self.lex_pos;
        self.comments
            .as_ref()
            .map_or(false, |pattern| !self.read_from_pos().starts_with(pattern))
    }

    fn read_from_pos(&self) -> String {
        self.buffer.iter().skip(self.lex_pos).collect()
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

    fn read_by_predicate(&mut self, pred: Predicate) -> String {
        self.advance_by_predicate(pred);

        self.buffer
            .iter()
            .skip(self.col_num)
            .take(self.lex_pos - self.col_num)
            .collect()
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
}

pub struct Token {
    pub name: String,
    pub loc: Loc,
}

impl Token {
    fn new(name: String, loc: Loc) -> Self {
        Self { name, loc }
    }
}

#[derive(Clone, Default)]
pub struct Loc {
    pub file: String,
    pub line: i32,
    pub col: i32,
}

impl Loc {
    pub fn new(file: String, line: i32, col: i32) -> Self {
        Self { file, line, col }
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.file.is_empty() {
            Ok(())
        } else {
            write!(f, "{}:{}:{}: ", self.file, self.line, self.col)
        }
    }
}
