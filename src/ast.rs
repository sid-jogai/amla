use std::hash::Hash;

type LineCol = (u32, u32);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
/// Position such that `&src[pos.0..pos.1]` is the underlying text.
pub struct Pos(usize, usize);

impl Pos {
    pub fn new(left: usize, right: usize) -> Pos {
        assert!(left <= right, "invalid pos {:?}", Pos(left, right));
        Pos(left, right)
    }
    pub fn l(&self) -> usize {
        self.0
    }
    pub fn r(&self) -> usize {
        self.1
    }
    pub fn show<'src>(&'src self, src: &'src str) -> String {
        src[self.0..self.1].to_string()
    }
    pub fn start(&self, src: &str) -> LineCol {
        src[..self.0]
            .chars()
            .fold((1, 0), |(line, col), c| match c {
                '\n' => (line + 1, 0),
                // '\t' => (line + 8, 0),
                _ => (line, col + 1),
            })
    }
    pub fn end(&self, src: &str) -> LineCol {
        src[..self.1]
            .chars()
            .fold((1, 0), |(line, col), c| match c {
                '\n' => (line + 1, 0),
                // '\t' => (line + 8, 0),
                _ => (line, col + 1),
            })
    }
    pub fn name_pos(&self, src: &str) -> NamePos {
        NamePos {
            name: self.show(src),
            pos: *self,
        }
    }
}

pub type AstId = u32;

#[derive(Debug, Clone)]
pub struct Stmt {
    pub id: AstId,
    pub pos: Pos,
    pub stmt: StmtKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    If(Box<If>),
    FnDecl(Box<Fn>),
    Return(Return),
    ExprStmt(Box<Expr>),
    Block(Vec<Stmt>),
    Assign(Box<Assign>),
    Update(Box<Update>),
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub yes: Box<Stmt>,
    pub no: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub name: NamePos,
    pub ty: TyPos,
    pub val: Box<Expr>,
}

// TODO: rename assign.val -> assign.expr

#[derive(Debug, Clone)]
pub struct Update {
    pub name: NamePos,
    pub val: Box<Expr>,
}

impl Assign {
    pub fn name(&self) -> &str {
        &self.name.name
    }
}

#[derive(Debug, Clone)]
pub struct Return(pub Option<Box<Expr>>);

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: NamePos,
    pub params: Vec<Param>,
    pub body: Box<Stmt>, /* StmtKind::Block. */
    pub ret: Option<TyPos>,
    pub sig: Pos,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: NamePos,
    pub ty: TyPos,
}

impl Fn {
    pub fn name(&self) -> &str {
        &self.name.name
    }
    pub fn ty(&self) -> Ty {
        let ty = self.params.iter().map(|p| p.ty.ty.clone()).collect();
        let ret = Box::from(self.ret.as_ref().map(|x| x.ty.clone()).unwrap_or(Ty::Void));
        Ty::Fn(ty, ret)
    }
    pub fn params(&self) -> Vec<Ty> {
        self.params.iter().map(|p| p.ty.ty.clone()).collect()
    }
    pub fn ret(&self) -> Ty {
        self.ret.as_ref().map(|x| x.ty.clone()).unwrap_or(Ty::Void)
    }
}

#[derive(Debug, Clone)]
pub struct NamePos {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub struct TyPos {
    pub ty: Ty,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub id: AstId,
    pub pos: Pos,
    pub ty: Ty,
    pub expr: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Identifier(Identifier),
    Literal(Literal),
    Call(Box<Call>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    I32,
    I64,
    Any,
    Int,
    Str,
    Bool,
    NoneYet,
    Void,
    Variadic(Box<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    Numeric, // For literals which can be coerced to more specific types
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Any => write!(f, "any"),
            Self::Int => write!(f, "int"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::Str => write!(f, "str"),
            Self::Bool => write!(f, "bool"),
            Self::NoneYet => write!(f, "?"),
            Self::Variadic(ty) => write!(f, "...{ty}"),
            Self::Void => write!(f, "void"),
            Self::Fn(params, ret) => {
                let ps = params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| match i == params.len() - 1 {
                        true => p.to_string(),
                        false => format!("{}, ", p),
                    })
                    .collect::<Vec<_>>()
                    .concat();
                write!(f, "({}) -> {}", ps, ret)
            }
            _ => todo!("display for {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Debug, Clone)]
pub struct Grouping(pub Expr);

#[derive(Debug, Clone)]
pub enum Literal {
    Number(i64),
    Bool(bool),
    Str(String),
}

#[derive(Debug, Clone)]
pub struct Call {
    pub caller: Box<Expr>,
    pub args: Vec<Expr>,
}

impl Call {
    pub fn caller_name(&self) -> Option<&str> {
        match &self.caller.expr {
            ExprKind::Identifier(Identifier(e)) => Some(e),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node<'a> {
    E(&'a Expr),
    S(&'a Stmt),
}

impl Node<'_> {
    pub fn id(&self) -> u32 {
        match self {
            Self::E(e) => e.id,
            Self::S(s) => s.id,
        }
    }
}
