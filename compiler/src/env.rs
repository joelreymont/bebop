use crate::error::Error;
use crate::ir::*;
use bebop_parser::ast;
use bebop_util::{id::*, meta::*};
use bitflags::bitflags;
use core::hash::Hash;
use ordermap::map::{Iter, IterMut, OrderMap};
use serde::{Serialize, Serializer};
use std::cell::RefCell;
use std::option::Option::*;

use std::rc::Rc;

bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Hash)]
    pub struct Kind: u8 {
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

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize)]
pub struct Key {
    id: Id,
    kind: Kind,
}

impl Key {
    fn new(id: Id, kind: Kind) -> Self {
        Self { id, kind }
    }

    pub fn id(&self) -> &Id {
        &self.id
    }

    pub fn kind(&self) -> &Kind {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Value {
    expr: ExprPtr,
}

impl Value {
    fn new(expr: ExprPtr) -> Self {
        Self { expr }
    }

    pub fn expr(&self) -> &ExprPtr {
        &self.expr
    }

    pub fn expr_mut(&mut self) -> &mut ExprPtr {
        &mut self.expr
    }
}

#[derive(Debug, PartialEq, Default, Serialize)]
pub struct TypeMap {
    inner: OrderMap<Key, Value>,
}

impl Clone for TypeMap {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl TypeMap {
    pub fn insert(&mut self, id: Id, expr: ExprPtr, kind: Kind) {
        self.inner.insert(Key::new(id, kind), Value::new(expr));
    }

    pub fn get(&self, id: &MetaId, kind: Kind) -> Option<ExprPtr> {
        let key = Key::new(*id.id(), kind);
        self.inner.get(&key).map(|x| x.expr.clone())
    }

    pub fn find(&self, id: &MetaId, mask: Kind) -> Option<ExprPtr> {
        self.find_iter(id, mask).next()
    }

    pub fn find_iter(
        &self,
        id: &MetaId,
        mask: Kind,
    ) -> impl Iterator<Item = ExprPtr> {
        self.inner
            .iter()
            .filter(move |(k, _)| {
                k.id == *id.id() && k.kind.intersects(mask)
            })
            .map(|(_, v)| v.expr.clone())
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, Key, Value> {
        self.inner.iter()
    }

    pub fn iter_mut<'a>(&'a mut self) -> IterMut<'a, Key, Value> {
        self.inner.iter_mut()
    }

    pub fn merge(&mut self, map: Self) {
        self.inner.extend(map.inner)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct MapPtr {
    map: Rc<RefCell<TypeMap>>,
}

impl Default for MapPtr {
    fn default() -> Self {
        Self::new(TypeMap::default())
    }
}

impl MapPtr {
    pub fn new(map: TypeMap) -> Self {
        Self {
            map: Rc::new(RefCell::new(map)),
        }
    }

    pub fn apply<T>(&self, fun: impl Fn(&TypeMap) -> T) -> T {
        fun(&self.map.borrow())
    }

    pub fn apply_mut<T>(
        &mut self,
        mut fun: impl FnMut(&mut TypeMap) -> T,
    ) -> T {
        fun(&mut self.map.borrow_mut())
    }
}

impl Serialize for MapPtr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.map.serialize(serializer)
    }
}

impl From<&MapPtr> for TypeMap {
    fn from(ptr: &MapPtr) -> Self {
        ptr.apply(|x| x.clone())
    }
}

#[derive(Debug, PartialEq, Default, Serialize)]
pub struct Env {
    // Enabling serialization of `parent_env` creates a circular reference
    // and overflows the stack. This is because `Rule.parent_env` points
    // to the global scope which embdeds the rule. We don't need to dump
    // the parent env of a rule anyway since it will be dumped as part
    // of the Architecture.
    #[serde(skip_serializing)]
    parent_env: Option<MapPtr>,
    env: MapPtr,
}

impl Clone for Env {
    fn clone(&self) -> Self {
        Self {
            parent_env: self.parent_env.clone(),
            env: self.env.clone(),
        }
    }
}

impl Env {
    pub fn to_local(&self) -> Self {
        Self {
            parent_env: Some(self.env.clone()),
            env: MapPtr::default(),
        }
    }

    pub fn empty(&self) -> Self {
        Self {
            parent_env: self.parent_env.clone(),
            env: MapPtr::default(),
        }
    }

    pub fn lookup(&self, id: &MetaId, kind: Kind) -> LiftResult {
        self.parent_env
            .as_ref()
            .and_then(|env| env.apply(|env| env.get(id, kind)))
            .or_else(|| self.env.apply(|env| env.get(id, kind)))
            .ok_or(Error::Unknown(*id))
    }

    pub fn insert(&mut self, id: MetaId, expr: ExprPtr, kind: Kind) {
        self.env
            .apply_mut(|env| env.insert(id.into_id(), expr.clone(), kind))
    }

    pub fn len(&self) -> usize {
        self.env.apply(|env| env.len())
    }

    pub fn is_empty(&self) -> bool {
        self.env.apply(|env| env.is_empty())
    }

    pub fn find(&self, id: &MetaId, mask: Kind) -> LiftResult {
        self.parent_env
            .as_ref()
            .and_then(|env| env.apply(|env| env.find(id, mask)))
            .or_else(|| self.env.apply(|env| env.find(id, mask)))
            .ok_or(Error::Unknown(*id))
    }

    pub fn apply_iter<T>(&self, fun: impl Fn(Iter<Key, Value>) -> T) -> T {
        self.env.apply(|env| fun(env.iter()))
    }

    pub fn apply_iter_mut<T>(
        &mut self,
        mut fun: impl FnMut(IterMut<Key, Value>) -> T,
    ) -> T {
        self.env.apply_mut(|env| fun(env.iter_mut()))
    }

    pub fn add_local_vars(
        &mut self,
        expr: &ast::Expr,
        expected_size: Option<usize>,
    ) -> Result<(), Error> {
        use ast::Expr::*;
        match expr {
            Sized { expr, size } => {
                let size = *size.value();
                let span = expr.span();
                if expected_size.is_some_and(|expected| size != expected) {
                    return Err(Error::SizeMismatch {
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
                let id = MetaId::from(id);
                let span = id.span();
                if self
                    .parent_env
                    .as_ref()
                    .and_then(|env| {
                        env.apply(|env| env.get(&id, Kind::Variable))
                    })
                    .or_else(|| {
                        self.env.apply(|env| env.get(&id, Kind::Variable))
                    })
                    .is_none()
                {
                    let var = Expr::Variable(Variable { id });
                    let expr = ExprPtr::new(var, span);
                    self.insert(id, expr, Kind::Variable);
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub fn import(&mut self, expr_ptr: &mut ExprPtr) -> Result<(), Error> {
        let mut expr_ptr1 = expr_ptr.clone();
        expr_ptr.apply_mut(|expr| self.import_expr(expr, &mut expr_ptr1))
    }

    fn import_target(
        &mut self,
        target: &mut JumpTarget,
    ) -> Result<(), Error> {
        use JumpTarget::*;
        match target {
            Direct(expr) => {
                let mut expr1 = expr.clone();
                expr.apply_mut(|x| self.import_expr(x, &mut expr1))?
            }
            Indirect(expr) => {
                let mut expr1 = expr.clone();
                expr.apply_mut(|x| self.import_expr(x, &mut expr1))?
            }
            _ => {}
        }
        Ok(())
    }

    fn import_expr(
        &mut self,
        expr: &mut Expr,
        expr_ptr: &mut ExprPtr,
    ) -> Result<(), Error> {
        match expr {
            Expr::Binary { lhs, rhs, .. } => {
                let mut lhs1 = lhs.clone();
                let mut rhs1 = rhs.clone();
                lhs.apply_mut(|x| self.import_expr(x, &mut lhs1))?;
                rhs.apply_mut(|x| self.import_expr(x, &mut rhs1))?;
            }
            Expr::Unary { rhs, .. } => {
                let mut rhs1 = rhs.clone();
                rhs.apply_mut(|x| self.import_expr(x, &mut rhs1))?
            }
            Expr::Paren(expr) => {
                let mut expr1 = expr.clone();
                expr.apply_mut(|x| self.import_expr(x, &mut expr1))?
            }
            Expr::Pointer { expr, .. } => {
                let mut expr1 = expr.clone();
                expr.apply_mut(|x| self.import_expr(x, &mut expr1))?
            }
            Expr::TakeBits { expr, .. } => {
                let mut expr1 = expr.clone();
                expr.apply_mut(|x| self.import_expr(x, &mut expr1))?
            }
            Expr::TakeBytes { expr, .. } => {
                let mut expr1 = expr.clone();
                expr.apply_mut(|x| self.import_expr(x, &mut expr1))?
            }
            Expr::FunCall { args, .. } => {
                for arg in args.iter_mut() {
                    let mut arg1 = arg.clone();
                    arg.apply_mut(|x| self.import_expr(x, &mut arg1))?;
                }
            }
            Expr::Variable(Variable { id }) => {
                self.insert(*id, expr_ptr.clone(), Kind::Variable)
            }
            Expr::Bind { lhs, rhs, .. } => {
                let mut lhs1 = lhs.clone();
                let mut rhs1 = rhs.clone();
                lhs.apply_mut(|x| self.import_expr(x, &mut lhs1))?;
                rhs.apply_mut(|x| self.import_expr(x, &mut rhs1))?;
            }
            Expr::Transfer(self::Transfer { target, .. }) => {
                self.import_target(target)?
            }
            Expr::Branch { condition, target } => {
                let mut condition1 = condition.clone();
                condition
                    .apply_mut(|x| self.import_expr(x, &mut condition1))?;
                self.import_target(target)?;
            }
            Expr::Export(expr) => {
                let mut expr1 = expr.clone();
                expr.apply_mut(|x| self.import_expr(x, &mut expr1))?
            }
            _ => {}
        }
        Ok(())
    }

    pub fn make_unique(&mut self) -> Result<(), Error> {
        let mut unique = TypeMap::default();
        self.env.apply_mut(|map| {
            for (k, v) in map.iter_mut() {
                let id = k.id().unique();
                unique.insert(id, v.expr().clone(), *k.kind());
                v.expr.apply_mut(|x| x.rename_variable(&id, x.span()))?;
            }
            Ok(())
        })?;
        self.env = MapPtr::new(unique);
        Ok(())
    }

    pub fn merge(&mut self, env: &Self) {
        let y = &env.env;
        self.env.apply_mut(|x| x.merge(y.into()))
    }
}
