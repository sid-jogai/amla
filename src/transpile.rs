use crate::ast;
use crate::ast::ExprKind;
use crate::ast::Literal;
use crate::ast::StmtKind;
use crate::ast::Ty;
use crate::err;

trait Transpile {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E>;
}

#[derive(Debug)]
struct Ctx {
    depth: i32,
}

impl Ctx {
    fn lbrace(&mut self, s: &mut String) {
        self.depth += 1;
        s.push('{');
        s.push('\n');
    }
    fn rbrace(&mut self, s: &mut String) {
        self.depth -= 1;
        for _ in 0..self.depth {
            s.push('\t');
        }
        s.push('}');
        // s.push('\n');
    }
}

pub fn transpile(ast: ast::Stmt) -> Result<String, err::E> {
    let mut s = String::new();
    let mut ctx = Ctx { depth: 0 };

    prelude(&mut ctx, &mut s);
    match ast.transpile(&mut ctx, &mut s) {
        Ok(_) => Ok(s),
        Err(err) => Err(err),
    }
}

impl Transpile for ast::Stmt {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        match &self.stmt {
            StmtKind::FnDecl(func) => {
                // Some functions are special cased by the compiler, but their
                // signatures are still written out so that they can be type
                // checked.
                if matches!(&func.name.name[..], "print") {
                    return Ok(());
                }

                func.transpile(ctx, s)?;
                s.push('\n');
            }
            StmtKind::Return(ret) => ret.transpile(ctx, s)?,
            StmtKind::Block(stmts) => {
                for stmt in stmts {
                    for _ in 0..ctx.depth {
                        s.push('\t');
                    }
                    stmt.transpile(ctx, s)?;
                    s.push('\n');
                }
            }
            StmtKind::If(r#if) => r#if.transpile(ctx, s)?,
            StmtKind::Assign(assign) => assign.transpile(ctx, s)?,
            StmtKind::ExprStmt(expr) => {
                expr.transpile(ctx, s)?;
                s.push(';');
            }
            other => todo!("implement transpile for {:?}", other),
        }
        Ok(())
    }
}

impl Transpile for ast::Fn {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        match &self.ret {
            Some(ty) => ty.ty.transpile(ctx, s)?,
            None => s.push_str("void"),
        };
        s.push('\n');
        s.push_str(&self.name.name);
        s.push_str(" (");
        if self.params.len() == 0 {
            s.push_str("void");
        }
        for (i, param) in self.params.iter().enumerate() {
            param.transpile(ctx, s)?;
            if i < self.params.len() - 1 {
                s.push_str(", ");
            }
        }
        s.push_str(")\n");
        ctx.lbrace(s);
        self.body.transpile(ctx, s)?;
        ctx.rbrace(s);
        Ok(())
    }
}

impl Transpile for ast::Param {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        self.ty.ty.transpile(ctx, s)?;
        s.push(' ');
        s.push_str(&self.name.name);
        Ok(())
    }
}

impl Transpile for ast::Ty {
    fn transpile(&self, _: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        match self {
            ast::Ty::Int => s.push_str("int"),
            ast::Ty::I32 => s.push_str("int32_t"),
            ast::Ty::I64 => s.push_str("int64_t"),
            _ => s.push_str("<type>"),
        }
        Ok(())
    }
}

impl Transpile for ast::Return {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        match &self.0 {
            None => s.push_str("return;"),
            Some(expr) => {
                s.push_str("return ");
                expr.transpile(ctx, s)?;
                s.push_str(";");
            }
        };
        Ok(())
    }
}

impl Transpile for ast::Expr {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        match &self.expr {
            ExprKind::Literal(literal) => {
                literal.transpile(ctx, s)?;
            }
            ExprKind::Identifier(identifier) => {
                identifier.transpile(ctx, s)?;
            }
            ExprKind::Call(call) => {
                if let ExprKind::Identifier(identifier) = &call.caller.expr {
                    match &identifier.0[..] {
                        "print" => print(self, &call.args, ctx, s)?,
                        _ => call.transpile(ctx, s)?,
                    }
                }
            }
        }
        Ok(())
    }
}

impl Transpile for Literal {
    fn transpile(&self, _: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        match self {
            Literal::Number(n) => s.push_str(&n.to_string()),
            Literal::Bool(false) => s.push_str("false"),
            Literal::Bool(true) => s.push_str("true"),
            Literal::Str(str) => {
                // TODO: escape sequences
                s.push('"');
                s.push_str(str);
                s.push('"');
            }
        };
        Ok(())
    }
}

impl Transpile for ast::Identifier {
    fn transpile(&self, _: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        s.push_str(&self.0);
        Ok(())
    }
}

impl Transpile for ast::Call {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        match &self.caller.expr {
            ExprKind::Identifier(identifier) => {
                // TODO: destructure to gensym_add which might do error checking.
                let op = match &identifier.0[..] {
                    "+" => Some(BinOpKind::Add),
                    "-" => Some(BinOpKind::Minus),
                    ">" => Some(BinOpKind::Greater),
                    "<" => Some(BinOpKind::Less),
                    "print" => {
                        return Ok(());
                    }
                    _ => None,
                };
                if let Some(op) = op {
                    let op = BinOp {
                        kind: op,
                        left: self.args[0].clone(),
                        right: self.args[1].clone(),
                    };
                    op.transpile(ctx, s)?;
                } else {
                    s.push_str(&identifier.0);
                    s.push('(');
                    for (i, param) in self.args.iter().enumerate() {
                        param.transpile(ctx, s)?;
                        if i < self.args.len() - 1 {
                            s.push_str(", ");
                        }
                    }
                    s.push(')');
                }
            }
            _ => {
                s.push_str(&format!("{:?}", self));
            }
        }
        Ok(())
    }
}

struct BinOp {
    kind: BinOpKind,
    left: ast::Expr,
    right: ast::Expr,
}

enum BinOpKind {
    Add,
    Minus,
    Greater,
    Less,
}

impl Transpile for BinOp {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        s.push('(');
        self.left.transpile(ctx, s)?;
        s.push(' ');
        self.kind.transpile(ctx, s)?;
        s.push(' ');
        self.right.transpile(ctx, s)?;
        s.push(')');
        Ok(())
    }
}

impl Transpile for BinOpKind {
    fn transpile(&self, _: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        match self {
            BinOpKind::Add => s.push('+'),
            BinOpKind::Minus => s.push('-'),
            BinOpKind::Greater => s.push('>'),
            BinOpKind::Less => s.push('<'),
        }
        Ok(())
    }
}

impl Transpile for ast::If {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        s.push_str("if (");
        self.cond.transpile(ctx, s)?;
        s.push_str(") ");
        ctx.lbrace(s);
        self.yes.transpile(ctx, s)?;
        ctx.rbrace(s);
        Ok(())
    }
}

impl Transpile for ast::Assign {
    fn transpile(&self, ctx: &mut Ctx, s: &mut String) -> Result<(), err::E> {
        self.ty.ty.transpile(ctx, s)?;
        s.push(' ');
        s.push_str(&self.name.name);
        s.push_str(" = ");
        self.val.transpile(ctx, s)?;
        s.push(';');
        Ok(())
    }
}

// TODO: maybe make these types instead of strings
fn prelude(_: &mut Ctx, s: &mut String) {
    let imports = [
        "<stdint.h>",   // int64_t, etc.
        "<inttypes.h>", // PRId64, etc.
        "<stdio.h>",    // printf
    ];

    for import in imports {
        s.push_str("#include ");
        s.push_str(import);
        s.push('\n');
    }

    s.push('\n');
    s.push('\n');
}

fn print(
    call_expr: &ast::Expr,
    args: &Vec<ast::Expr>,
    ctx: &mut Ctx,
    s: &mut String,
) -> Result<(), err::E> {
    let format_string = match &args[0].expr {
        ExprKind::Literal(Literal::Str(s)) => s,
        _ => unreachable!(),
    };
    let mut c_format_string = String::new();
    let parts: Vec<_> = format_string.match_indices("%v").collect();
    if parts.len() != args.len() - 1 {
        return Err(err::E {
            text: "wrong number of format arguments".to_string(),
            source: call_expr.pos,
            kind: err::ErrorKind::TypeError,
            notes: vec![],
        });
    }
    let mut i = 0;
    for (j, part) in parts.into_iter().enumerate() {
        c_format_string.push('"');
        c_format_string.push_str(&format_string[i..part.0]);

        let format_specifier = match &args[j + 1].ty {
            Ty::NoneYet | Ty::I32 => "PRId32",
            Ty::I64 => "PRId64",
            _ => todo!("format specifier for {:?}", args[j + 1].ty),
        };
        println!("!!! {:?}", args[j + 1]);
        c_format_string.push_str("%\" ");
        c_format_string.push_str(format_specifier);
        c_format_string.push(' ');

        i = part.0 + 2;
    }
    println!("All {:?}", args);
    c_format_string.push('"');
    c_format_string.push_str(&format_string[i..]);
    c_format_string.push_str("\\n\"");

    if args.len() > 1 {
        c_format_string.push_str(", "); // TODO
    }

    s.push_str("printf(");
    s.push_str(&c_format_string);
    for (i, arg) in args.iter().enumerate() {
        if i == 0 {
            continue;
        }

        arg.transpile(ctx, s)?;
        if i < args.len() - 1 {
            s.push_str(", ");
        }
    }
    s.push_str(")");
    Ok(())
}
