use crate::hir::*;
use bebop_util::meta::*;
use core::hash::{Hash, Hasher};
use core::ops;
use serde::{Serialize, Serializer};

#[derive(Copy, Clone, Debug)]
pub struct ExprId(Loc<id_arena::Id<Expr>>);

impl ExprId {
    pub fn with_span(&self, span: Span) -> Self {
        Self(self.0.map_tag(|_| span))
    }
}

impl Hash for ExprId {
    #[inline]
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.0.value().hash(h);
    }
}

impl PartialEq for ExprId {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        self.0.value() == rhs.0.value()
    }
}

impl Eq for ExprId {}

impl Spanned for ExprId {
    fn span(&self) -> Span {
        *self.0.tag()
    }
}

#[derive(Debug)]
pub struct ExprPool {
    arena: id_arena::Arena<Expr>,
}

impl Default for ExprPool {
    fn default() -> Self {
        Self {
            arena: id_arena::Arena::<Expr>::new(),
        }
    }
}

impl ExprPool {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn add(&mut self, expr: Expr, span: Span) -> ExprId {
        ExprId(Loc::new(self.arena.alloc(expr), span))
    }
}

impl ops::Index<&ExprId> for ExprPool {
    type Output = Expr;

    #[inline]
    fn index(&self, id: &ExprId) -> &Expr {
        &self.arena[id.0.into_value()]
    }
}

impl ops::IndexMut<&ExprId> for ExprPool {
    #[inline]
    fn index_mut(&mut self, id: &ExprId) -> &mut Expr {
        &mut self.arena[id.0.into_value()]
    }
}

impl Serialize for ExprPool {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let a: Vec<(usize, &Expr)> = self
            .arena
            .iter()
            .map(|(id, expr)| (id.index(), expr))
            .collect();
        a.serialize(serializer)
    }
}

impl Serialize for ExprId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.index().serialize(serializer)
    }
}
