use crate::ast::Pos;
use crate::ast::Ty;
use std::fmt::Write;

pub struct Source {
    pub filename: String,
    pub text: String,
}

pub struct E {
    pub text: String,
    pub source: Pos,
    pub kind: ErrorKind,
    pub notes: Vec<Note>,
}

impl E {
    // e.g. x: i32 = "hello"
    pub fn invalid_assignment_error(assign_stmt: Pos, annotation_ty: &Ty, expr_ty: &Ty) -> E {
        E {
            text: format!("cannot assign {:?} to {:?}", expr_ty, annotation_ty),
            source: assign_stmt,
            kind: ErrorKind::TypeError,
            notes: vec![],
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    SyntaxError,
    TypeError,
}

#[derive(Debug)]
pub enum Note {
    Footnote(String),
    Note { text: String, pos: Pos },
}

pub fn new(error: &E, source: &Source) -> String {
    let (line, col) = error.source.start(&source.text);
    let (start, end) = (error.source.l(), error.source.r());
    let kind = match error.kind {
        ErrorKind::SyntaxError => "Syntax Error",
        ErrorKind::TypeError => "Type Error",
    };
    let offset = " ".repeat(col as usize);
    let underline = "^".repeat(end - start);
    let mut s = format!(
        "{} on line {}, column {} in file {}\n\n",
        kind, line, col, source.filename
    );
    writeln!(&mut s, "{:>4} | {}", line, getline(source, line)).unwrap();
    writeln!(&mut s, "     | {}{}", offset, underline).unwrap();
    writeln!(&mut s, "     | {}{}", offset, error.text).unwrap();
    for note in &error.notes {
        fmt_note(source, note, &mut s);
    }
    s
}

fn getline(source: &Source, line: u32) -> &str {
    // 1-indexed.
    source.text.lines().nth(line as usize - 1).unwrap()
}

fn fmt_note(source: &Source, note: &Note, s: &mut String) {
    match note {
        Note::Footnote(text) => {
            writeln!(s, "     |").unwrap();
            writeln!(s, "     | {}", text).unwrap();
        }
        Note::Note { text, pos } => {
            let (line, col) = pos.start(&source.text);
            let offset = " ".repeat(col as usize);
            let underline = "^".repeat(pos.r() - pos.l());
            writeln!(s, "\n{:>4} | {}", line, getline(source, line)).unwrap();
            writeln!(s, "     | {}{}", offset, underline).unwrap();
            writeln!(s, "     | {}{}", offset, text).unwrap();
        }
    }
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
