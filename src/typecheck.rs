use crate::ast;
use crate::ast::Expr;
use crate::ast::ExprKind;
use crate::ast::Pos;
use crate::ast::Stmt;
use crate::ast::StmtKind;
use crate::ast::Ty;
use crate::err;
use crate::err::E;
use std::collections::HashMap;

pub fn typecheck(ast: &mut Stmt) -> Result<(), err::E> {
    let mut m = TypeMap::new();

    match typecheck_stmt(ast, &mut m) {
        Err(e) => Err(e),
        _ => Ok(()),
    }
}

#[derive(Debug)]
struct TypeMap {
    v: Vec<HashMap<String, Ty>>,
}

impl TypeMap {
    fn new() -> TypeMap {
        let v = Vec::new();
        TypeMap { v }
    }
    fn push_scope(&mut self) {
        let m = HashMap::new();
        self.v.push(m);
    }
    fn pop_scope(&mut self) {
        self.v.pop();
    }
    fn lookup(&mut self, s: &str) -> Option<ast::Ty> {
        for scope in self.v.iter().rev() {
            if let Some(ty) = scope.get(s) {
                return Some(ty.clone());
            }
        }
        None
    }
    fn insert(&mut self, s: String, ty: ast::Ty) -> Option<ast::Ty> {
        let i = self.v.len() - 1;
        self.v[i].insert(s, ty)
    }
}

fn typecheck_stmt(stmt: &mut Stmt, m: &mut TypeMap) -> Result<(), err::E> {
    match &mut stmt.stmt {
        //             StmtKind::If(_) => Ok(()),
        //             StmtKind::ExprStmt(x) => self.resolve_expr(x),
        StmtKind::Return(ret) => {
            if let Some(ret_expr) = &mut ret.0 {
                typecheck_expr(ret_expr, m)?;
            }
            // TODO!
            Ok(())
        }
        StmtKind::Block(stmts) => {
            m.push_scope();
            for stmt in stmts {
                typecheck_stmt(stmt, m)?;
            }
            m.pop_scope();
            Ok(())
        }
        StmtKind::Assign(assign) => {
            // TODO: support simple type inference
            let ty = typecheck_expr(&mut assign.val, m)?;

            let ty = match (&assign.ty.ty, ty) {
                (Ty::I64, Ty::Numeric) => Ty::I64,
                (Ty::I32, Ty::Numeric) => Ty::I32,
                (annotation, got) => {
                    if got == *annotation {
                        got.clone()
                    } else {
                        return Err(E::invalid_assignment_error(stmt.pos, annotation, &got));
                    }
                }
            };

            m.insert(assign.name.name.clone(), ty.clone());

            Ok(())
        }
        StmtKind::FnDecl(func) => {
            m.insert(func.name.name.clone(), func.ty());

            m.push_scope();
            for param in &func.params {
                m.insert(param.name.name.clone(), param.ty.ty.clone());
                // TODO: check for duplicates
            }
            typecheck_stmt(&mut func.body, m)?;
            m.pop_scope();
            Ok(())
        }
        StmtKind::ExprStmt(expr) => {
            let ty = typecheck_expr(expr, m)?;
            expr.ty = ty;
            Ok(())
        }

        other => todo!("implement typechecker for {:?}", other),
    }
}

fn typecheck_expr(expr: &mut Expr, m: &mut TypeMap) -> Result<ast::Ty, err::E> {
    let expr_ty = match &mut expr.expr {
        ExprKind::Identifier(ast::Identifier(s)) => match m.lookup(s) {
            None => Err(undeclared_identifier_err(expr.pos, s)),
            Some(ty) => Ok(ty),
        },
        ExprKind::Literal(literal) => Ok(match literal {
            ast::Literal::Str(_) => Ty::Str,
            ast::Literal::Number(_) => Ty::Numeric,
            ast::Literal::Bool(_) => Ty::Bool,
        }),
        ExprKind::Call(call) => {
            //
            let fn_ty = typecheck_expr(&mut call.caller, m)?;
            let caller_name = match &call.caller.expr {
                ExprKind::Identifier(ast::Identifier(fn_name)) => fn_name,
                _ => &"<function object>".to_string(),
            };

            let (params, ret) = match fn_ty {
                Ty::Fn(ref params, ref ret) => (params, ret.clone()),
                _ => return Err(bad_caller_err(call.caller.pos)),
            };
            let args = &mut call.args;

            let mut variadic = false;

            for (i, param) in params.iter().enumerate() {
                if let Ty::Variadic(_) = param {
                    if args.len() < i - 1 {
                        let footnote = format!("{caller_name}: {fn_ty}");
                        return Err(incorrect_fn_args_count_error(expr.pos, &footnote));
                    }
                } else if i == params.len() - 1 && args.len() < params.len() {
                    let footnote = format!("{caller_name}: {fn_ty}");
                    return Err(incorrect_fn_args_count_error(expr.pos, &footnote));
                }
            }

            let mut param_index = 0;
            for arg in args {
                if !variadic && param_index >= params.len() {
                    let footnote = format!("{caller_name}: {fn_ty}");
                    return Err(incorrect_fn_args_count_error(expr.pos, &footnote));
                }

                let param_ty = &params[param_index];
                let arg_ty = typecheck_expr(arg, m)?;

                let param_ty = match param_ty {
                    Ty::Variadic(ty) => {
                        // TODO: check for multiple variadic parameters here.
                        variadic = true;
                        ty
                    }
                    ty => ty,
                };
                if !variadic {
                    param_index += 1;
                }

                if *param_ty != Ty::Any && *param_ty != arg_ty {
                    let footnote = format!("{caller_name}: {fn_ty}");
                    return Err(ty_missmatch_err(param_ty, &arg_ty, arg.pos, &footnote));
                }
            }
            Ok(*ret)
        }
    };

    expr.ty = expr_ty?;
    Ok(expr.ty.clone())
}

fn ty_missmatch_err(param_ty: &Ty, arg_ty: &Ty, arg_pos: Pos, fn_string: &str) -> err::E {
    err::E {
        text: format!("expected {}, found {}", param_ty, arg_ty),
        source: arg_pos,
        kind: err::ErrorKind::TypeError,
        notes: vec![err::Note::Footnote(format!("note: {fn_string}"))],
    }
}

// fn expected_return_err(ret_pos: Pos, exp_ty: &Ty, fn_pos: Pos) -> err::E {
//     err::E {
//         text: format!(
//             "empty return; expected an expression of type {}",
//             exp_ty
//         ),
//         source: ret_pos,
//         kind: err::ErrorKind::TypeError,
//         notes: vec![err::Note::Note {
//             text: "note: expected due to this".to_string(),
//             pos: fn_pos,
//         }],
//     }
// }

fn undeclared_identifier_err(pos: Pos, name: &str) -> err::E {
    err::E {
        text: format!("undeclared identifier \"{}\"", name),
        source: pos,
        kind: err::ErrorKind::TypeError,
        notes: vec![],
    }
}

// fn return_stmt_outside_fn_err(pos: Pos) -> err::E {
//     err::E {
//         text: "return statement outside of function".to_string(),
//         source: pos,
//         kind: err::ErrorKind::SyntaxError,
//         notes: vec![],
//     }
// }
//
// fn nested_fn_err(pos: Pos) -> err::E {
//     err::E {
//         text: "nested functions not allowed".to_string(),
//         source: pos,
//         kind: err::ErrorKind::SyntaxError,
//         notes: vec![],
//     }
// }
//
// fn identifier_exists_err(pos: Pos, name: String, prev_pos: Pos) -> err::E {
//     err::E {
//         text: "identifier already exists".to_string(),
//         source: pos,
//         kind: err::ErrorKind::SyntaxError,
//         notes: vec![err::Note::Note {
//             text: format!("note: \"{}\" previously declared here", name),
//             pos: prev_pos,
//         }],
//     }
// }

fn bad_caller_err(pos: Pos) -> err::E {
    err::E {
        text: "not a function".to_string(),
        source: pos,
        kind: err::ErrorKind::SyntaxError,
        notes: vec![],
    }
}

fn incorrect_fn_args_count_error(pos: Pos, footnote: &str) -> err::E {
    let text = "incorrect number of arguments supplied".to_string();
    err::E {
        text,
        source: pos,
        kind: err::ErrorKind::SyntaxError,
        notes: vec![err::Note::Footnote(format!("note: {footnote}"))],
    }
}
