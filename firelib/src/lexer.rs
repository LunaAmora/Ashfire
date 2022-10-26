use core::fmt;
use std::{
    collections::HashMap,
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
    matches: Vec<Match>,
    separators: Vec<char>,
    comments: Option<String>,
}

impl LexerBuilder {
    fn new(file: PathBuf) -> Self {
        Self { file, ..Default::default() }
    }

    /// Separators are not included in other tokens, unless inside a [`Match`].
    pub fn with_separators(mut self, sep: Vec<char>) -> Self {
        self.separators.extend(sep);
        self
    }

    /// Captures everything betwen a pair of chars or until the end of the line.
    pub fn with_matches(mut self, matches: Vec<Match>) -> Self {
        self.matches.extend(matches);
        self
    }

    /// Makes the rest of the line be ignored by the lexer.
    pub fn with_comments(mut self, comment: &str) -> Self {
        self.comments = Some(comment.to_owned());
        self
    }

    /// Tries to build an `Lexer` with the given file and parameters.
    pub fn build(self) -> Result<Lexer> {
        let file = File::open(&self.file)
            .with_context(|| format!("Could not read file `{:?}`", self.file))?;

        let mut matches = HashMap::with_capacity(self.matches.len());
        for value in &self.matches {
            match value {
                Match::Same(start) => matches.insert(*start, *start),
                Match::Pair(start, end) => matches.insert(*start, *end),
            };
        }

        let reader = BufReader::new(file);
        Ok(Lexer::new(reader, self.file, self.separators, self.comments, matches))
    }
}

pub enum Match {
    Same(char),
    Pair(char, char),
}

enum Predicate {
    Char(char),
    Whitespace,
    Separators,
}

pub struct Lexer {
    buffer: Vec<char>,
    reader: BufReader<File>,
    separators: Vec<char>,
    comments: Option<String>,
    matches: HashMap<char, char>,
    file: PathBuf,
    lex_pos: usize,
    col_num: usize,
    line_num: i32,
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl Lexer {
    pub fn new(
        reader: BufReader<File>, file: PathBuf, separators: Vec<char>, comments: Option<String>,
        matches: HashMap<char, char>,
    ) -> Self {
        Self {
            reader,
            file,
            separators,
            comments,
            matches,
            buffer: Vec::new(),
            lex_pos: 0,
            col_num: 0,
            line_num: 0,
        }
    }

    /// Returns an builder object for working with the `Lexer`.
    pub fn builder(file: &PathBuf) -> LexerBuilder {
        LexerBuilder::new(file.to_owned())
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
        while let Some(&buf) = self.buffer.get(self.lex_pos) {
            match pred {
                Predicate::Char(char) if buf == char => return,
                Predicate::Whitespace if buf != ' ' => return,
                Predicate::Separators if buf == ' ' => return,

                Predicate::Separators if self.is_comment(buf) => return,

                Predicate::Separators if self.separators.contains(&buf) => {
                    if self.lex_pos > self.col_num {
                        return;
                    } else {
                        self.lex_pos += 1
                    }
                }

                Predicate::Separators => match self.matches.get(&buf) {
                    Some(end) => {
                        self.lex_pos += 1;
                        self.advance_by_predicate(Predicate::Char(*end));
                        self.lex_pos += 1;
                        return;
                    }
                    _ => self.lex_pos += 1,
                },

                _ => self.lex_pos += 1,
            };
        }
    }

    fn is_comment(&self, buf: char) -> bool {
        match &self.comments {
            Some(comment) if comment.starts_with(buf) => self.check_match_next(comment),
            _ => false,
        }
    }

    fn check_match_next(&self, matcher: &str) -> bool {
        for (index, c) in matcher.char_indices().skip(1) {
            if !self
                .buffer
                .get(self.lex_pos + index)
                .map_or(false, |&next| c == next)
            {
                return false;
            }
        }
        return true;
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
