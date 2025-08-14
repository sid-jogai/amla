pub mod ast;
pub mod err;
pub mod lex;
pub mod parse;
pub mod transpile;
pub mod typecheck;

use std::fs;
use std::process::Command;

#[derive(Debug)]
enum Cmd {
    Transpile(String),
    Lex(String),
    Parse(String),
    Compile(String),
}

fn parse_cmd(mut args: std::env::Args) -> Option<Cmd> {
    args.next();
    match (args.next(), args.next()) {
        (Some(file), None) => Some(Cmd::Transpile(file)),
        (Some(arg), Some(file)) => match &arg[..] {
            "--lex" => Some(Cmd::Lex(file)),
            "--parse" => Some(Cmd::Parse(file)),
            "--transpile" => Some(Cmd::Transpile(file)),
            "--compile" => Some(Cmd::Compile(file)),
            _ => None,
        },
        _ => None,
    }
}

fn lex(file: &str) {
    let src = std::fs::read_to_string(file).unwrap_or_else(|_| panic!("Error reading {file}"));
    let show = err::ErrorDisplay {
        src: &src,
        filename: file,
    };
    let tokens = match lex::lex(&src) {
        Ok(tokens) => tokens,
        Err(errors) => {
            for err in &errors {
                println!("{}", show.make_error(err));
            }
            return;
        }
    };
    for t in tokens {
        println!("{:?}", t);
    }
}

fn parse(file: &str) {
    let src = std::fs::read_to_string(file).unwrap_or_else(|_| panic!("Error reading {file}"));
    let show = err::ErrorDisplay {
        src: &src,
        filename: file,
    };
    let tokens = match lex::lex(&src) {
        Ok(tokens) => tokens,
        Err(errors) => {
            for err in &errors {
                println!("{}", show.make_error(err));
            }
            return;
        }
    };
    let ast = match parse::parse(&src, &tokens) {
        Err(err) => {
            println!("{}", show.make_error(&err));
            return;
        }
        Ok(ast) => ast,
    };
    println!("{:?}", ast);
}

fn transpile(file: &str) {
    #[rustfmt::skip]
    let src = std::fs::read_to_string(file)
        .unwrap_or_else(|_| panic!("Error reading {file}"));
    let show = err::ErrorDisplay {
        src: &src,
        filename: file,
    };
    let tokens = match lex::lex(&src) {
        Ok(tokens) => tokens,
        Err(errors) => {
            for err in &errors {
                println!("{}", show.make_error(err));
            }
            return;
        }
    };
    let mut ast = match parse::parse(&src, &tokens) {
        Err(err) => {
            println!("{}", show.make_error(&err));
            return;
        }
        Ok(ast) => ast,
    };
    if let Err(err) = typecheck::typecheck(&mut ast) {
        println!("{}", show.make_error(&err));
        return;
    }
    let source = match transpile::transpile(ast) {
        Err(err) => {
            println!("{}", show.make_error(&err));
            return;
        }
        Ok(source) => source,
    };
    println!("{}", source);
}

fn compile(file: &str) {
    #[rustfmt::skip]
    let src = std::fs::read_to_string(file)
        .unwrap_or_else(|_| panic!("Error reading {file}"));
    let show = err::ErrorDisplay {
        src: &src,
        filename: file,
    };
    let tokens = match lex::lex(&src) {
        Ok(tokens) => tokens,
        Err(errors) => {
            for err in &errors {
                println!("{}", show.make_error(err));
            }
            return;
        }
    };
    let ast = match parse::parse(&src, &tokens) {
        Err(err) => {
            println!("{}", show.make_error(&err));
            return;
        }
        Ok(ast) => ast,
    };
    let source = match transpile::transpile(ast) {
        Err(err) => {
            println!("{}", show.make_error(&err));
            return;
        }
        Ok(source) => source,
    };

    fs::write("generated.c", source).expect("Write error");

    let output = Command::new("cc")
        .arg("-Wall")
        .arg("-Wextra")
        .arg("-Wpedantic")
        .arg("generated.c")
        .output()
        .expect("Compiler error");

    let stdout = String::from_utf8_lossy(&output.stdout);
    if !stdout.trim().is_empty() {
        println!("{}", stdout);
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.trim().is_empty() {
        println!("{}", stderr);
    };

    // if output.status.success() {
    //     println!("WTF{}", String::from_utf8_lossy(&output.stderr));
    //     let stdout = String::from_utf8_lossy(&output.stdout);
    //     if !stdout.trim().is_empty() {
    //         println!("stdout: {}", stdout);
    //     };
    // } else {
    //     println!("1WTF{}", String::from_utf8_lossy(&output.stderr));
    //     println!("{}", String::from_utf8_lossy(&output.stderr));
    // }
}

pub fn main() {
    match parse_cmd(std::env::args()) {
        Some(Cmd::Lex(file)) => lex(&file),
        Some(Cmd::Parse(file)) => parse(&file),
        Some(Cmd::Transpile(file)) => transpile(&file),
        Some(Cmd::Compile(file)) => compile(&file),
        _ => (),
    };
}
