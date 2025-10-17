use std::fmt::Display;

use egglog::{
    EGraph, Value,
    ast::{Expr, GenericExpr, Literal, RustSpan, Span},
    span,
};

use crate::wrap::{EgglogNode, Sym};

/// 2 ways to access database of EGraph
/// one is direct access by Value
/// another is indirect access by SymLit(SymOrLiteral) which read the database to get corresponding
/// value of node/base type
/// So we use this trait to unify 2 ways upon
/// It's important to distinguish them because we could never get a value of node  
/// before commited, in that way we should represent them by Sym in place of Value.
pub trait EValue {
    fn get_value_by_eval_string(&self, egraph: &mut EGraph) -> Value;
    fn get_egglog_expr(&self) -> GenericExpr<String, String>;
    fn get_symlit(&self) -> SymLit;
}

pub enum SymLit {
    Sym(Sym),
    Lit(Literal),
}

impl<T: EgglogNode> EValue for T {
    /// warnning: this function can't be used before the node is commited to egraph
    ///
    /// This function is slow because we have to parse the expression
    fn get_value_by_eval_string(&self, egraph: &mut EGraph) -> Value {
        self.cur_sym().get_value_by_eval_string(egraph)
    }
    fn get_egglog_expr(&self) -> Expr {
        self.cur_sym().get_egglog_expr()
    }

    fn get_symlit(&self) -> SymLit {
        SymLit::Sym(self.cur_sym())
    }
}
impl EValue for Sym {
    fn get_value_by_eval_string(&self, egraph: &mut EGraph) -> Value {
        egraph.eval_expr(&self.get_egglog_expr()).unwrap().1
    }
    fn get_egglog_expr(&self) -> Expr {
        Expr::Call(span!(), self.to_string(), vec![])
    }
    fn get_symlit(&self) -> SymLit {
        SymLit::Sym(*self)
    }
}

pub fn get_func_value(egraph: &mut EGraph, name: &str, args: Box<[&dyn EValue]>) -> Value {
    log::trace!("get_value_of_func {}", name);
    let (_, expr_value) = egraph
        .eval_expr(&egglog::ast::GenericExpr::Call(
            span!(),
            name.to_string(),
            args.iter().map(|y| y.get_egglog_expr()).collect(),
        ))
        .unwrap();
    expr_value
}

impl EValue for i64 {
    fn get_value_by_eval_string(&self, egraph: &mut EGraph) -> Value {
        egraph.base_to_value(*self)
    }
    fn get_egglog_expr(&self) -> GenericExpr<String, String> {
        GenericExpr::Lit(span!(), egglog::ast::Literal::Int(*self))
    }
    fn get_symlit(&self) -> SymLit {
        SymLit::Lit(Literal::Int(*self))
    }
}
impl EValue for String {
    fn get_value_by_eval_string(&self, egraph: &mut EGraph) -> Value {
        egraph.base_to_value(self.clone())
    }
    fn get_egglog_expr(&self) -> GenericExpr<String, String> {
        GenericExpr::Lit(span!(), egglog::ast::Literal::String(self.clone()))
    }
    fn get_symlit(&self) -> SymLit {
        SymLit::Lit(Literal::String(self.clone()))
    }
}
impl EValue for bool {
    fn get_value_by_eval_string(&self, egraph: &mut EGraph) -> Value {
        egraph.base_to_value(*self)
    }
    fn get_egglog_expr(&self) -> GenericExpr<String, String> {
        GenericExpr::Lit(span!(), egglog::ast::Literal::Bool(*self))
    }
    fn get_symlit(&self) -> SymLit {
        SymLit::Lit(Literal::Bool(*self))
    }
}
// !todo!
// impl EValue for f32{
//     fn get_value(&self, egraph:&mut EGraph) -> Value {
//         egraph.base_to_value(OrderedFloat(*self))
//     }
//     fn get_eexpr(&self) -> GenericExpr<String,String> {
//         GenericExpr::Lit(span!(), egglog::ast::Literal::Int(*self))
//     }
//     fn get_symlit(&self) -> SymLit {
//         SymLit::Lit(Literal::Float(*self))
//     }
// }
impl Display for SymLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SymLit::Sym(sym) => {
                    sym.inner.to_string()
                }
                SymLit::Lit(literal) => {
                    match literal {
                        Literal::Int(i) => {
                            format!("{}", i)
                        }
                        Literal::Float(f) => {
                            format!("{}", f)
                        }
                        Literal::String(s) => {
                            format!("{}", s)
                        }
                        Literal::Bool(b) => b.to_string(),
                        Literal::Unit => {
                            format!("()")
                        }
                    }
                }
            }
        )
    }
}
