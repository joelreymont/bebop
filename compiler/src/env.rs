use crate::error::LiftError;
use crate::hir::*;
use bebop_parser::ast;
use bebop_util::meta::*;
use bitflags::bitflags;
use core::hash::Hash;
use ordermap::map::{Iter, IterMut, OrderMap};
use serde::Serialize;
use std::option::Option::*;

bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Hash)]
    pub struct Types: u8 {
        const MemoryRegion  = 0b00000001;
        const Register      = 0b00000010;
        const RegisterIndex = 0b00000100;
        const BitField      = 0b00001000;
        const Intrinsic     = 0b00010000;
        const Macro         = 0b00100000;
        const Scanner       = 0b01000000;
        const Variable      = 0b10000000;
    }
}

// #[derive(Debug, Clone, Serialize)]
// struct Entry(Types, ExprPtr);
type Key = (Ident, Types);
type Env = OrderMap<Key, ExprPtr>;

#[derive(Debug, PartialEq, Serialize)]
pub struct TypeEnv {
    env: Env,
}

impl Clone for TypeEnv {
    fn clone(&self) -> Self {
        Self {
            env: self.env.clone(),
        }
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self {
            env: OrderMap::new(),
        }
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, id: Id, expr: ExprPtr, types: Types) {
        self.env.insert((*id.ident(), types), expr);
    }

    pub fn get(&self, id: &Id, types: Types) -> Option<ExprPtr> {
        let key = (*id.ident(), types);
        self.env.get(&key).cloned()
    }

    pub fn find(&self, id: &Id, mask: Types) -> Option<ExprPtr> {
        self.env
            .iter()
            .filter(move |((ident, types), _)| {
                ident == id.ident() && types.intersects(mask)
            })
            .map(|(_, expr)| expr.clone())
            .next()
    }

    pub fn len(&self) -> usize {
        self.env.len()
    }

    pub fn is_empty(&self) -> bool {
        self.env.is_empty()
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, Key, ExprPtr> {
        self.env.iter()
    }

    pub fn iter_mut<'a>(&'a mut self) -> IterMut<'a, Key, ExprPtr> {
        self.env.iter_mut()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Scope {
    // Enabling serialization of `parent_env` creates a circular reference
    // and overflows the stack. This is because `Rule.parent_env` points
    // to the global scope which embdeds the rule. We don't need to dump
    // the parent env of a rule anyway since it will be dumped as part
    // of the Architecture.
    #[serde(skip_serializing)]
    pub parent_env: Option<TypeEnv>,
    pub env: TypeEnv,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            parent_env: None,
            env: TypeEnv::new(),
        }
    }
}

impl Scope {
    pub fn to_local(&self) -> Self {
        Self {
            parent_env: Some(self.env.clone()),
            env: TypeEnv::new(),
        }
    }

    pub fn lookup(&self, id: &Id, types: Types) -> LiftResult {
        self.parent_env
            .as_ref()
            .and_then(|env| env.get(id, types))
            .or_else(|| self.env.get(id, types))
            .ok_or(LiftError::Unknown(*id))
    }

    pub fn insert(&mut self, id: Id, expr: ExprPtr, types: Types) {
        self.env.insert(id, expr, types)
    }

    pub fn len(&self) -> usize {
        self.env.len()
    }

    pub fn is_empty(&self) -> bool {
        self.env.is_empty()
    }

    pub fn find(&self, id: &Id, mask: Types) -> LiftResult {
        self.parent_env
            .as_ref()
            .and_then(|env| env.find(id, mask))
            .or_else(|| self.env.find(id, mask))
            .ok_or(LiftError::Unknown(*id))
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, Key, ExprPtr> {
        self.env.iter()
    }

    pub fn iter_mut<'a>(&'a mut self) -> IterMut<'a, Key, ExprPtr> {
        self.env.iter_mut()
    }

    pub fn add_local_vars(
        &mut self,
        expr: &ast::Expr,
        expected_size: Option<usize>,
    ) -> Result<(), LiftError> {
        use ast::Expr::*;
        match expr {
            Sized { expr, size } => {
                let size = *size.value();
                let span = expr.span();
                if expected_size.is_some_and(|expected| size != expected) {
                    return Err(LiftError::SizeMismatch {
                        span,
                        want: expected_size.unwrap(),
                        got: size,
                    });
                }
                self.add_local_vars(expr, Some(size))?
            }
            Pointer { expr, .. } => {
                self.add_local_vars(expr, expected_size)?
            }
            Id(id) => {
                let id: self::Id = id.into();
                let span = id.span();
                if self
                    .parent_env
                    .as_ref()
                    .and_then(|env| env.get(&id, Types::Variable))
                    .or_else(|| self.env.get(&id, Types::Variable))
                    .is_none()
                {
                    let var = Expr::Variable(Variable { id });
                    let expr = ExprPtr::new(var, span);
                    self.insert(id, expr, Types::Variable);
                }
            }
            _ => {}
        }
        Ok(())
    }
}
