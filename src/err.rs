use crate::ast::Pos;
use std::fmt::Write;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct LineInfo {
    line: u32,
    col: u32,
}

#[derive(Debug)]
pub enum ErrorKind {
    SyntaxError,
    TypeError,
}

#[derive(Debug)]
pub struct E {
    pub text: String,
    pub source: Pos,
    pub kind: ErrorKind,
    pub notes: Vec<Note>,
}

#[derive(Debug)]
pub enum Note {
    Footnote(String),
    Note { text: String, pos: Pos },
}

macro_rules! syntax_error {
    ($text:expr, $pos:expr) => {
        err::E {
            text: $text,
            source: $pos,
            kind: err::ErrorKind::SyntaxError,
            notes: vec![],
        }
    };
    ($text:expr, $pos:expr, $footnote:expr) => {
        err::E {
            text: $text,
            source: $pos,
            kind: err::ErrorKind::SyntaxError,
            notes: vec![err::Note::Footnote($footnote)],
        }
    };
    ($text:expr, $pos:expr, $note_text:expr, $note_pos:expr) => {
        err::E {
            text: $text,
            source: $pos,
            kind: err::ErrorKind::SyntaxError,
            notes: vec![err::Note::Note {
                text: $note_text,
                pos: $note_pos,
            }],
        }
    };
}
pub(crate) use syntax_error;

pub struct ErrorDisplay<'a> {
    pub src: &'a str,
    pub filename: &'a str,
}

impl<'a> ErrorDisplay<'a> {
    pub fn make_error(&self, error: &E) -> String {
        let (line, col) = error.source.start(self.src);
        let (start, end) = (error.source.l(), error.source.r());
        let kind = match error.kind {
            ErrorKind::SyntaxError => "Syntax Error",
            ErrorKind::TypeError => "Type Error",
        };
        let offset = " ".repeat(col as usize);
        let underline = "^".repeat(end - start);
        let mut s = format!(
            "{} on line {}, column {} in file {}\n\n",
            kind, line, col, self.filename
        );
        writeln!(&mut s, "{:>4} | {}", line, self.getline(line)).unwrap();
        writeln!(&mut s, "     | {}{}", offset, underline).unwrap();
        writeln!(&mut s, "     | {}{}", offset, error.text).unwrap();
        for note in &error.notes {
            self.fmt_note(&mut s, note);
        }
        s
    }
    fn fmt_note(&self, s: &mut String, note: &Note) {
        match note {
            Note::Footnote(text) => {
                writeln!(s, "     |").unwrap();
                writeln!(s, "     | {}", text).unwrap();
            }
            Note::Note { text, pos } => {
                let (line, col) = pos.start(self.src);
                let offset = " ".repeat(col as usize);
                let underline = "^".repeat(pos.r() - pos.l());
                writeln!(s, "\n{:>4} | {}", line, self.getline(line)).unwrap();
                writeln!(s, "     | {}{}", offset, underline).unwrap();
                writeln!(s, "     | {}{}", offset, text).unwrap();
            }
        }
    }
    fn getline(&self, line: u32) -> &str {
        /* One-indexed. */
        self.src.lines().nth(line as usize - 1).unwrap()
    }
}
