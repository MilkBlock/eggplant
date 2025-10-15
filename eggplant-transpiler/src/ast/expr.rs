use super::*;

impl<Head: Clone + Display, Leaf: Clone + Display + Eq + std::hash::Hash> GenericExpr<Head, Leaf> {
    pub fn span(&self) -> Span {
        match self {
            GenericExpr::Lit(span, _) => span.clone(),
            GenericExpr::Var(span, _) => span.clone(),
            GenericExpr::Call(span, _, _) => span.clone(),
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, GenericExpr::Var(_, _))
    }

    pub fn get_var(&self) -> Option<Leaf> {
        match self {
            GenericExpr::Var(_ann, v) => Some(v.clone()),
            _ => None,
        }
    }

    fn children(&self) -> &[Self] {
        match self {
            GenericExpr::Var(_, _) | GenericExpr::Lit(_, _) => &[],
            GenericExpr::Call(_, _, children) => children,
        }
    }

    pub fn ast_size(&self) -> usize {
        let mut size = 0;
        self.walk(&mut |_e| size += 1, &mut |_| {});
        size
    }

    pub fn walk(&self, pre: &mut impl FnMut(&Self), post: &mut impl FnMut(&Self)) {
        pre(self);
        self.children()
            .iter()
            .for_each(|child| child.walk(pre, post));
        post(self);
    }

    pub fn fold<Out>(&self, f: &mut impl FnMut(&Self, Vec<Out>) -> Out) -> Out {
        let ts = self.children().iter().map(|child| child.fold(f)).collect();
        f(self, ts)
    }

    pub fn visit_exprs(self, f: &mut impl FnMut(Self) -> Self) -> Self {
        match self {
            GenericExpr::Lit(..) => f(self),
            GenericExpr::Var(..) => f(self),
            GenericExpr::Call(span, op, children) => {
                let children = children.into_iter().map(|c| c.visit_exprs(f)).collect();
                f(GenericExpr::Call(span, op.clone(), children))
            }
        }
    }

    pub fn subst<Head2, Leaf2>(
        &self,
        subst_leaf: &mut impl FnMut(&Span, &Leaf) -> GenericExpr<Head2, Leaf2>,
        subst_head: &mut impl FnMut(&Head) -> Head2,
    ) -> GenericExpr<Head2, Leaf2> {
        match self {
            GenericExpr::Lit(span, lit) => GenericExpr::Lit(span.clone(), lit.clone()),
            GenericExpr::Var(span, v) => subst_leaf(span, v),
            GenericExpr::Call(span, op, children) => {
                let children = children
                    .iter()
                    .map(|c| c.subst(subst_leaf, subst_head))
                    .collect();
                GenericExpr::Call(span.clone(), subst_head(op), children)
            }
        }
    }

    pub fn subst_leaf<Leaf2>(
        &self,
        subst_leaf: &mut impl FnMut(&Span, &Leaf) -> GenericExpr<Head, Leaf2>,
    ) -> GenericExpr<Head, Leaf2> {
        self.subst(subst_leaf, &mut |x| x.clone())
    }

    pub fn vars(&self) -> impl Iterator<Item = Leaf> + '_ {
        let iterator: Box<dyn Iterator<Item = Leaf>> = match self {
            GenericExpr::Lit(_ann, _l) => Box::new(std::iter::empty()),
            GenericExpr::Var(_ann, v) => Box::new(std::iter::once(v.clone())),
            GenericExpr::Call(_ann, _head, exprs) => Box::new(exprs.iter().flat_map(|e| e.vars())),
        };
        iterator
    }
}
