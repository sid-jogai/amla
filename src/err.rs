use crate::ast::Pos;
use crate::ast::Ty;
use crate::lex::TokenType;
use std::fmt::Write;

use crate::err::ErrorKind::SyntaxError;
use crate::err::ErrorKind::TypeError;

pub struct Source {
    pub filename: String,
    pub text: String,
}

pub struct E {
    text: String,
    source: Pos,
    kind: ErrorKind,
    notes: Vec<Note>,
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

// Syntax Errors
impl E {
    // ../test/fail/syntax_error/unterminated_string_literal.amla
    pub fn unterminated_string_literal(literal: Pos) -> E {
        E {
            text: "unterminated string literal".to_string(),
            source: literal,
            kind: SyntaxError,
            notes: vec![],
        }
    }
    // ../test/fail/syntax_error/unterminated_character_literal.amla
    pub fn unterminated_character_literal(literal: Pos) -> E {
        E {
            text: "unterminated character literal".to_string(),
            source: literal,
            kind: SyntaxError,
            notes: vec![],
        }
    }
    // ../test/fail/syntax_error/unrecognized_character.amla
    pub fn unrecognized_character(c: char, pos: Pos) -> E {
        E {
            text: format!("unrecognized character {c}"),
            source: pos,
            kind: SyntaxError,
            notes: vec![],
        }
    }
    // ../test/fail/syntax_error/expected_top_level_statement.amla
    pub fn expected_top_level_statement(pos: Pos) -> E {
        E {
            text: "expected top level statement".to_string(),
            source: pos,
            kind: SyntaxError,
            notes: vec![Note::Footnote("expected one of 'func'".to_string())],
        }
    }
    // ../test/fail/syntax_error/expected_token.amla
    pub fn expected_token(got: TokenType, expected: TokenType, pos: Pos) -> E {
        E {
            // TODO: don't use the debug formatter but instead show the character
            text: format!("unexpected {:?}, exected {:?}", got, expected),
            source: pos,
            kind: SyntaxError,
            notes: vec![],
        }
    }
    // ../test/fail/syntax_error/expected_expression.amla
    pub fn expected_expression(pos: Pos) -> E {
        E {
            text: "expected expression".to_string(),
            source: pos,
            kind: SyntaxError,
            notes: vec![],
        }
    }
    // ../test/fail/syntax_error/expected_type.amla
    pub fn expected_type(pos: Pos) -> E {
        E {
            text: "expected type".to_string(),
            source: pos,
            kind: SyntaxError,
            notes: vec![],
        }
    }
    // ../test/fail/syntax_error/numeric_literal_exceeds_u64_max.amla
    pub fn numeric_literal_exceeds_u64_max(pos: Pos) -> E {
        E {
            text: format!("numeric literal exceeds u64 max ({})", u64::MAX),
            source: pos,
            kind: SyntaxError,
            notes: vec![],
        }
    }
}

// Type Errors
impl E {
    // ../test/fail/type_error/print_wrong_number_of_format_arguments.amla
    pub fn print_wrong_number_of_format_arguments(pos: Pos) -> E {
        E {
            text: "wrong number of format arguments".to_string(),
            source: pos,
            kind: TypeError,
            notes: vec![],
        }
    }
    // ../test/fail/type_error/invalid_assignment.amla
    pub fn invalid_assignment(annotation_ty: &Ty, expr_ty: &Ty, assign_stmt: Pos) -> E {
        E {
            text: format!("cannot assign {:?} to {:?}", expr_ty, annotation_ty),
            source: assign_stmt,
            kind: TypeError,
            notes: vec![],
        }
    }
    // ../test/fail/type_error/not_a_function.amla
    pub fn not_a_function(pos: Pos) -> E {
        E {
            text: "not a function".to_string(),
            source: pos,
            kind: TypeError,
            notes: vec![],
        }
    }
    //  ../test/fail/type_error/undeclared_identifier.amla
    pub fn undeclared_identifier(name: &str, pos: Pos) -> E {
        E {
            text: format!("undeclared identifier \"{}\"", name),
            source: pos,
            kind: TypeError,
            notes: vec![],
        }
    }
    //  ../test/fail/type_error/incorrect_argument_count1.amla
    //  ../test/fail/type_error/incorrect_argument_count2.amla
    pub fn incorrect_argument_count(footnote: &str, pos: Pos) -> E {
        let text = "incorrect number of arguments supplied".to_string();
        E {
            text,
            source: pos,
            kind: TypeError,
            notes: vec![Note::Footnote(format!("note: {footnote}"))],
        }
    }
    //  ../test/fail/type_error/type_missmatch.amla
    pub fn type_missmatch(param_ty: &Ty, arg_ty: &Ty, fn_string: &str, arg_pos: Pos) -> E {
        E {
            text: format!("expected {}, found {}", param_ty, arg_ty),
            source: arg_pos,
            kind: TypeError,
            notes: vec![Note::Footnote(format!("note: {fn_string}"))],
        }
    }
}

// TODO: work with tabs!

pub fn new(error: &E, source: &Source) -> String {
    let (line, col) = error.source.start(&source.text);
    let (start, end) = (error.source.l(), error.source.r());
    let kind = match error.kind {
        SyntaxError => "Syntax Error",
        TypeError => "Type Error",
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

fn visual_col(line: &str, col: usize) -> usize {
    let mut i = 0;
    println!("Here in {line}");
    for (j, c) in line[..col].chars().enumerate() {
        if j >= i {
            break;
        }
        i += if c == '\t' {
            println!("tab case");
            8
        } else {
            1
        }
    }
    i
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
