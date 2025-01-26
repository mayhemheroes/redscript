use hashbrown::HashSet;
use redscript_ast::{self as ast, Span, Spanned};
use thiserror::Error;

#[derive(Debug)]
pub struct Evaluator<'a> {
    modules: HashSet<Box<[&'a str]>>,
}

impl<'a> Evaluator<'a> {
    pub fn new(modules: HashSet<Box<[&'a str]>>) -> Self {
        Self { modules }
    }

    pub fn eval(&self, (expr, span): &Spanned<ast::SourceExpr<'_>>) -> Result<Value, Error> {
        let res = match expr {
            ast::Expr::Constant(ast::Constant::Bool(b)) => Value::Bool(*b),
            ast::Expr::BinOp { lhs, op, rhs } => match (self.eval(lhs)?, self.eval(rhs)?) {
                (Value::Bool(lhs), Value::Bool(rhs)) => match op {
                    ast::BinOp::And => Value::Bool(lhs && rhs),
                    ast::BinOp::Or => Value::Bool(lhs || rhs),
                    _ => return Err(Error::UnsupportedOperation(*span)),
                },
            },
            ast::Expr::UnOp { op, expr } => match (op, self.eval(expr)?) {
                (ast::UnOp::Not, Value::Bool(b)) => Value::Bool(!b),
                _ => return Err(Error::UnsupportedOperation(*span)),
            },
            ast::Expr::Call {
                expr,
                type_args,
                args,
            } => match (&**expr, &**type_args, &**args) {
                (
                    (ast::Expr::Ident("ModuleExists"), _),
                    [],
                    [(ast::Expr::Constant(ast::Constant::String(str)), _)],
                ) => {
                    let path = str.split('.').collect::<Vec<_>>();
                    Value::Bool(self.modules.contains(&path[..]))
                }
                _ => return Err(Error::UnsupportedOperation(*span)),
            },
            _ => return Err(Error::UnsupportedOperation(*span)),
        };
        Ok(res)
    }
}

pub enum Value {
    Bool(bool),
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("unsupported operation")]
    UnsupportedOperation(Span),
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Self::UnsupportedOperation(span) => *span,
        }
    }
}
