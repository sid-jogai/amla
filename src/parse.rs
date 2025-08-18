// TODO: remove this!
#![allow(unused)]

use crate::ast;
use crate::ast::ExprKind;
use crate::ast::Pos;
use crate::ast::StmtKind;
use crate::ast::Ty;
use crate::err;
use crate::err::E;
use crate::err::Source;
use crate::lex;
use crate::lex::TokenType;

pub fn parse(source: &Source, tokens: &[lex::Token]) -> Result<ast::Stmt, err::E> {
    let parser = Parser::new(&source.text, tokens);
    parser.parse_prog()
}

struct Parser<'a> {
    src: &'a str,
    pub tokens: &'a [lex::Token],
    pub pos: usize,
    id: u32,
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Prec {
    Assign,
    Rel,
    Term,
    Factor,
    Neg,
    Group,
    Call,
    Invalid,
    Max,
}

fn prefix_prec(tok_kind: TokenType) -> Prec {
    match tok_kind {
        TokenType::Identifier | TokenType::Number | TokenType::True | TokenType::False => {
            Prec::Assign
        }
        TokenType::Minus => Prec::Neg,
        TokenType::Lparen => Prec::Group,
        _ => Prec::Invalid,
    }
}

fn infix_prec(tok_kind: TokenType) -> Prec {
    match tok_kind {
        TokenType::Plus | TokenType::Minus => Prec::Term,
        TokenType::Star | TokenType::Slash | TokenType::Percent => Prec::Factor,
        TokenType::Lparen => Prec::Call,
        TokenType::BangEq
        | TokenType::EqEq
        | TokenType::Geq
        | TokenType::Gt
        | TokenType::Leq
        | TokenType::Lt => Prec::Rel,
        _ => Prec::Invalid,
    }
}

impl<'a> Parser<'a> {
    fn new(src: &'a str, tokens: &'a [lex::Token]) -> Self {
        Parser {
            src,
            tokens,
            pos: 0,
            id: 0,
        }
    }
    fn new_id(&mut self) -> u32 {
        self.id += 1;
        self.id - 1
    }
    fn consume(&mut self) -> lex::Token {
        self.pos += 1;
        self.tokens[self.pos - 1]
    }
    fn eat(&mut self, tk: TokenType) -> bool {
        if tk == self.peek_kind() {
            self.consume();
            return true;
        }
        false
    }
    fn peek_tok(&self) -> lex::Token {
        self.tokens[self.pos]
    }
    fn peek_kind(&self) -> TokenType {
        self.tokens[self.pos].ty
    }
    fn pos(&self) -> Pos {
        self.tokens[self.pos].pos
    }
    fn expect(&mut self, tt: TokenType) -> Result<lex::Token, err::E> {
        let got = self.peek_tok();
        if got.ty == tt {
            Ok(self.consume())
        } else {
            Err(E::expected_token(got.ty, tt, got.pos))
        }
    }
    fn parse_prog(mut self) -> Result<ast::Stmt, err::E> {
        let mut prog = Vec::new();
        loop {
            let stmt = match self.peek_kind() {
                TokenType::EOF => break,
                TokenType::Func => self.fn_decl(),
                _ => {
                    return Err(E::expected_top_level_statement(self.pos()));
                }
            };
            match stmt {
                Err(e) => return Err(e),
                Ok(stmt) => prog.push(stmt),
            }
        }

        assert!(self.pos + 1 == self.tokens.len());
        Ok(ast::Stmt {
            id: self.new_id(),
            pos: Pos::new(0, 0), /* For now. */
            stmt: StmtKind::Block(prog),
        })
    }
    fn parse_stmt(&mut self) -> Result<ast::Stmt, err::E> {
        let tok = self.peek_tok();
        let stmt = match tok.ty {
            TokenType::Return => self.return_stmt(),
            TokenType::If => return self.if_stmt(TokenType::If),
            TokenType::Func => return self.fn_decl(),
            TokenType::Identifier => {
                // TODO expr_stmt
                return self.parse_assignment();
            }
            _ => self.expr_stmt(),
        };
        stmt
    }
    // Either a assignment/update or a simple statement expression.
    fn parse_assignment(&mut self) -> Result<ast::Stmt, err::E> {
        let expr = self.expr()?;
        let name = expr.pos.name_pos(self.src);
        let assign = self.peek_tok();
        let is_assignment = match assign.ty {
            // TODO: type inference with :=
            lex::TokenType::Eq => false,
            lex::TokenType::Colon => true,
            _ => {
                self.expect(TokenType::Semicolon)?;
                return Ok(ast::Stmt {
                    id: self.new_id(),
                    pos: expr.pos,
                    stmt: StmtKind::ExprStmt(Box::new(expr)),
                });
            }
        };
        self.consume();
        let mut ty = ast::TyPos {
            ty: Ty::NoneYet,
            pos: Pos::new(0, 0),
        };
        if is_assignment {
            ty = self.ty()?;
            self.expect(TokenType::Eq)?;
        }
        let val = Box::new(self.expr()?);

        self.expect(TokenType::Semicolon)?;

        Ok(ast::Stmt {
            id: self.new_id(),
            pos: Pos::new(name.pos.l(), val.pos.r()),
            stmt: if is_assignment {
                StmtKind::Assign(Box::new(ast::Assign { name, ty, val }))
            } else {
                StmtKind::Update(Box::new(ast::Update { name, val }))
            },
        })
    }
    fn fn_decl(&mut self) -> Result<ast::Stmt, err::E> {
        let def = self.expect(TokenType::Func)?;
        let name = self.expect(TokenType::Identifier)?;
        let name = name.pos.name_pos(self.src);
        self.expect(TokenType::Lparen)?;

        let mut params = Vec::new();
        let rparen = match self.peek_kind() {
            TokenType::Rparen => self.consume(),
            _ => {
                params = self.param_list()?;
                self.expect(TokenType::Rparen)?
            }
        };

        let mut ret = None;
        let tok = self.peek_tok();
        if tok.ty != TokenType::Lbrace {
            ret = Some(self.ty()?);
        }
        let sig = Pos::new(def.pos.l(), tok.pos.r());
        let body = self.suite()?;
        Ok(ast::Stmt {
            id: self.new_id(),
            pos: Pos::new(def.pos.l(), body.pos.r()),
            stmt: StmtKind::FnDecl(Box::new(ast::Fn {
                name,
                params,
                body,
                ret,
                sig,
            })),
        })
    }
    fn if_stmt(&mut self, tt: TokenType) -> Result<ast::Stmt, err::E> {
        let start = self.expect(tt)?;
        let cond = Box::new(self.expr()?);

        let block_start = self.expect(TokenType::Lbrace)?;

        let yes = self.suite()?;
        let mut no = None;
        match self.peek_kind() {
            TokenType::Else => {
                self.expect(TokenType::Else)?;
                self.expect(TokenType::Colon)?;
                no = Some(self.suite()?);
            }
            _ => (),
        }
        Ok(ast::Stmt {
            id: self.new_id(),
            pos: Pos::new(start.pos.l(), yes.pos.r()),
            stmt: StmtKind::If(Box::new(ast::If {
                cond,
                yes: yes.clone(),
                no,
            })),
        })
    }
    fn param_list(&mut self) -> Result<Vec<ast::Param>, err::E> {
        let mut params = Vec::new();
        loop {
            let is_variadic = self.peek_tok().ty == TokenType::Ellipsis;
            if is_variadic {
                self.consume();
            }
            // TODO: only last parameter can be variadic

            let name = self.expect(TokenType::Identifier)?;
            /* TODO: Error message if the user forgot. */
            if matches!(self.peek_tok().ty, TokenType::Rparen | TokenType::Comma) {
                let pk = self.peek_tok();
                return Err(E::expected_type(pk.pos));
            }

            self.expect(TokenType::Colon)?;

            let mut ty = self.ty()?;
            if is_variadic {
                ty = ast::TyPos {
                    ty: Ty::Variadic(Box::new(ty.ty)),
                    pos: ty.pos,
                };
            }
            if self.peek_kind() == TokenType::Rparen {
                params.push(ast::Param {
                    name: ast::NamePos {
                        name: name.pos.show(self.src).to_string(),
                        pos: name.pos,
                    },
                    ty,
                });
                break;
            }
            self.expect(TokenType::Comma)?;
            params.push(ast::Param {
                name: ast::NamePos {
                    name: name.pos.show(self.src).to_string(),
                    pos: name.pos,
                },
                ty,
            });
        }
        Ok(params)
    }
    fn suite(&mut self) -> Result<Box<ast::Stmt>, err::E> {
        let start = self.expect(TokenType::Lbrace)?;
        let mut v = Vec::new();
        loop {
            if self.peek_kind() == TokenType::Rbrace {
                break;
            }
            let stmt = self.parse_stmt()?;
            v.push(stmt);
        }
        let end = self.expect(TokenType::Rbrace)?;
        let end_pos = match &v[..] {
            [.., x] => x.pos.r(),
            [] => end.pos.r(),
        };

        Ok(Box::new(ast::Stmt {
            id: self.new_id(),
            pos: Pos::new(start.pos.l(), end_pos),
            stmt: StmtKind::Block(v),
        }))
    }
    fn ty(&mut self) -> Result<ast::TyPos, err::E> {
        let tok = self.expect(TokenType::Identifier)?;
        match tok.ty {
            TokenType::Identifier => {
                let ty = match &tok.pos.show(self.src)[..] {
                    "int" => Ty::Int,
                    "i32" => Ty::I32,
                    "i64" => Ty::I64,
                    "str" => Ty::Str,
                    "bool" => Ty::Bool,
                    "any" => Ty::Any,
                    _ => {
                        return Err(E::expected_type(tok.pos));
                    }
                };
                Ok(ast::TyPos { ty, pos: tok.pos })
            }
            _ => unreachable!(),
        }
    }
    fn return_stmt(&mut self) -> Result<ast::Stmt, err::E> {
        let start = self.expect(TokenType::Return)?.pos.l();
        let (pos, expr) = match self.peek_kind() {
            TokenType::Semicolon => {
                let end = self.expect(TokenType::Semicolon)?;
                let end = end.pos.r();
                (Pos::new(start, end), None)
            }
            _ => {
                let expr = Box::new(self.expr()?);
                let end = self.expect(TokenType::Semicolon)?.pos.r();
                (Pos::new(start, end), Some(expr))
            }
        };
        Ok(ast::Stmt {
            id: self.new_id(),
            pos,
            stmt: StmtKind::Return(ast::Return(expr)),
        })
    }
    fn expr_stmt(&mut self) -> Result<ast::Stmt, err::E> {
        let expr = self.expr()?;
        Ok(ast::Stmt {
            id: self.new_id(),
            pos: expr.pos,
            stmt: StmtKind::ExprStmt(Box::new(expr)),
        })
    }
    fn expr(&mut self) -> Result<ast::Expr, err::E> {
        self.parse_expr(Prec::Assign)
    }
    fn parse_expr(&mut self, prec: Prec) -> Result<ast::Expr, err::E> {
        let tok = self.peek_tok();
        if !matches!(
            tok.ty,
            TokenType::Identifier
                | TokenType::Number
                | TokenType::Minus
                | TokenType::Lparen
                | TokenType::Str
                | TokenType::True
                | TokenType::False
        ) {
            return Err(E::expected_expression(tok.pos));
        }
        let mut tok = self.consume();

        let mut left = self.prefix_expr(tok)?;
        let new_prec = infix_prec(self.peek_tok().ty);
        while prec < infix_prec(self.peek_kind()) {
            if infix_prec(self.peek_kind()) == Prec::Invalid {
                return Ok(left);
            }
            tok = self.consume();
            left = self.infix_expr(left, tok)?
        }
        Ok(left)
    }
    fn mk_expr(&mut self, pos: Pos, expr: ast::ExprKind) -> ast::Expr {
        ast::Expr {
            id: self.new_id(),
            ty: Ty::NoneYet,
            pos,
            expr,
        }
    }
    fn prefix_expr(&mut self, tok: lex::Token) -> Result<ast::Expr, err::E> {
        match tok.ty {
            TokenType::Minus => {
                let prec = prefix_prec(tok.ty);
                let rhs = self.parse_expr(prec)?;

                let caller = Box::new(ast::Expr {
                    id: self.new_id(),
                    pos: tok.pos,
                    ty: Ty::NoneYet, /* Maybe add this. */
                    expr: ExprKind::Identifier(ast::Identifier(tok.pos.show(self.src))),
                });
                let args = vec![rhs.clone()];
                Ok(ast::Expr {
                    id: self.new_id(),
                    pos: Pos::new(tok.pos.l(), rhs.pos.r()),
                    ty: Ty::NoneYet,
                    expr: ExprKind::Call(Box::new(ast::Call { caller, args })),
                })
            }
            TokenType::Identifier => {
                let name = tok.pos.show(self.src).to_string();
                Ok(ast::Expr {
                    id: self.new_id(),
                    pos: tok.pos,
                    ty: Ty::NoneYet,
                    expr: ExprKind::Identifier(ast::Identifier(name)),
                })
            }
            TokenType::Number => {
                let value: u64 = match tok.pos.show(self.src).chars().collect::<String>().parse() {
                    Err(err) => {
                        return Err(E::numeric_literal_exceeds_u64_max(tok.pos));
                    }
                    Ok(v) => v,
                };
                Ok(ast::Expr {
                    id: self.new_id(),
                    pos: tok.pos,
                    ty: Ty::NoneYet,
                    expr: ExprKind::Literal(ast::Literal::Number(value as i64)),
                })
            }
            t @ (TokenType::True | TokenType::False) => {
                let expr = ast::Literal::Bool(t == TokenType::True);
                Ok(ast::Expr {
                    id: self.new_id(),
                    pos: tok.pos,
                    ty: Ty::NoneYet,
                    expr: ExprKind::Literal(expr),
                })
            }

            TokenType::Lparen => {
                let expr = self.parse_expr(Prec::Assign)?;

                let rparen = self.expect(TokenType::Rparen)?;
                Ok(ast::Expr {
                    id: expr.id,
                    ty: Ty::NoneYet,
                    pos: Pos::new(tok.pos.l(), rparen.pos.r()),
                    expr: expr.expr,
                })
            }
            TokenType::Str => {
                let np = Pos::new(tok.pos.l() + 1, tok.pos.r() - 1);
                let value = np.show(self.src);
                Ok(ast::Expr {
                    id: self.new_id(),
                    ty: Ty::NoneYet,
                    pos: tok.pos,
                    expr: ExprKind::Literal(ast::Literal::Str(value)),
                })
            }
            other => todo!("TODO {:?}", other),
        }
    }
    fn infix_expr(&mut self, lhs: ast::Expr, tok: lex::Token) -> Result<ast::Expr, err::E> {
        match tok.ty {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Slash
            | TokenType::BangEq
            | TokenType::Percent
            | TokenType::EqEq
            | TokenType::Geq
            | TokenType::Gt
            | TokenType::Leq
            | TokenType::Lt => {
                let rhs = self.parse_expr(infix_prec(tok.ty))?;
                let caller = Box::new(ast::Expr {
                    id: self.new_id(),
                    pos: tok.pos,
                    ty: Ty::NoneYet, /* Maybe add this. */
                    expr: ExprKind::Identifier(ast::Identifier(tok.pos.show(self.src))),
                });
                let args = vec![lhs.clone(), rhs.clone()];
                Ok(ast::Expr {
                    id: self.new_id(),
                    pos: Pos::new(lhs.pos.l(), rhs.pos.r()),
                    ty: Ty::NoneYet,
                    expr: ExprKind::Call(Box::new(ast::Call { caller, args })),
                })
            }
            TokenType::Lparen => {
                let mut args = Vec::new();

                let end = if self.peek_kind() == TokenType::Rparen {
                    self.expect(TokenType::Rparen)?.pos.r()
                } else {
                    loop {
                        let expr = self.parse_expr(Prec::Assign)?;
                        args.push(expr);
                        if self.peek_kind() == TokenType::Rparen {
                            break;
                        }
                        self.expect(TokenType::Comma)?;
                    }
                    self.expect(TokenType::Rparen)?.pos.r()
                };

                Ok(ast::Expr {
                    id: self.new_id(),
                    pos: Pos::new(lhs.pos.l(), end),
                    ty: Ty::NoneYet,
                    expr: ExprKind::Call(Box::new({
                        ast::Call {
                            caller: Box::new(lhs),
                            args,
                        }
                    })),
                })
            }
            _ => todo!(),
        }
    }
}
