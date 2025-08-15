pub mod ast;
pub mod err;
pub mod lex;
pub mod parse;
pub mod transpile;
pub mod typecheck;

use err::E;
use err::Source;

use std::io;
use std::io::Write;
use std::process::Command;

#[derive(PartialEq)]
enum Cmd {
    Lex,
    Parse,
    Compile,
    Run,
    Usage,
}

fn usage() -> ! {
    println!("Usage: amla [command]\n");
    println!("Commands:");
    println!("\tlex");
    println!("\tparse");
    println!("\tcompile");
    println!("\trun");
    std::process::exit(0);
}

fn die(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(1);
}

// TODO: figure out a module system.
//
// Until then, just splice in the prelude AST.
fn add_prelude(ast: ast::Stmt) -> Result<ast::Stmt, E> {
    let prelude_str = include_str!("prelude.amla");
    let prelude_source = Source {
        filename: "prelude.amla".to_string(),
        text: prelude_str.to_string(),
    };
    let prelude_tokens = lex::lex(&prelude_source)?;
    let prelude_ast = parse::parse(&prelude_source, &prelude_tokens)?;

    let stmt = match (prelude_ast.stmt, ast.stmt) {
        (ast::StmtKind::Block(mut prelude_stmts), ast::StmtKind::Block(mut stmts)) => {
            prelude_stmts.append(&mut stmts);
            ast::StmtKind::Block(prelude_stmts)
        }
        _ => unreachable!(),
    };
    let ast = ast::Stmt {
        id: ast.id,
        pos: ast.pos,
        stmt: stmt,
    };
    Ok(ast)
}

fn go(cmd: &Cmd, source: &Source) -> Result<(), err::E> {
    match cmd {
        Cmd::Lex => {
            for token in lex::lex(source)? {
                println!("{:?}", token);
            }
        }
        Cmd::Parse => {
            let tokens = lex::lex(source)?;
            let ast = parse::parse(source, &tokens)?;
            println!("{:#?}", ast);
        }
        Cmd::Compile => {
            let tokens = lex::lex(source)?;
            let ast = parse::parse(source, &tokens)?;
            let mut ast = add_prelude(ast)?;
            typecheck::typecheck(&mut ast)?;
            let c_code = transpile::transpile(ast)?;
            println!("{c_code}"); // NOTE: for debugging.

            if let Err(err) = transpile::compile(c_code) {
                die(&err);
            };
        }
        Cmd::Run => {
            let tokens = lex::lex(source)?;
            let ast = parse::parse(source, &tokens)?;
            let mut ast = add_prelude(ast)?;
            typecheck::typecheck(&mut ast)?;
            let c_code = transpile::transpile(ast)?;
            if let Err(err) = transpile::compile(c_code) {
                die(&err);
            };

            let result = Command::new("./a.out").output().unwrap();
            let stdout = String::from_utf8_lossy(&result.stdout);
            print!("{}", stdout);
            io::stdout().flush().unwrap();
        }
        _ => usage(),
    }
    Ok(())
}

fn open_file(path: &str) -> String {
    match std::fs::read_to_string(path) {
        Ok(text) => text,
        Err(err) => {
            die(&format!("Error opening {}: {}", path, err));
        }
    }
}

pub fn main() {
    let mut args = std::env::args();
    args.next();
    let (cmd, filename) = match (args.next(), args.next()) {
        (Some(arg), Some(file)) => match &arg[..] {
            "lex" => (Cmd::Lex, file),
            "parse" => (Cmd::Parse, file),
            "compile" => (Cmd::Compile, file),
            "run" => (Cmd::Run, file),
            _ => (Cmd::Usage, "".to_string()),
        },
        _ => (Cmd::Usage, "".to_string()),
    };
    if cmd == Cmd::Usage {
        usage();
    }

    let text = open_file(&filename);
    let source = Source { filename, text };

    if let Err(e) = go(&cmd, &source) {
        eprintln!("{}", err::new(&e, &source));
    }
}
