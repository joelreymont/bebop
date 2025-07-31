use crate::meta::{Loc, Span, Spanned, Tagged};
use core::sync::atomic::{self, AtomicUsize};
use internment::Intern;
use serde::{Serialize, Serializer};
use std::{fmt, ops::Deref};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn reset_unique_id_counter() {
    COUNTER.store(0, atomic::Ordering::SeqCst);
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Id(Intern<String>);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "`{}`", self.0)
    }
}

impl Id {
    pub fn new<S: ToString>(s: S) -> Self {
        Self(Intern::new(s.to_string()))
    }

    pub fn unique(&self) -> Self {
        let n = COUNTER.fetch_add(1, atomic::Ordering::SeqCst);
        let mut s = String::from(&*self.0);
        s.push_str(&n.to_string());
        Self::new(s)
    }
}

impl Deref for Id {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<String> for Id {
    fn as_ref(&self) -> &'static String {
        self.0.as_ref()
    }
}

impl Serialize for Id {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = self.0;
        s.serialize(serializer)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct LocId(Tagged<Id, Span>);

impl LocId {
    pub fn new(id: Id, span: Span) -> Self {
        Self(Tagged::new(id, span))
    }

    pub fn id(&self) -> &Id {
        self.0.value()
    }

    pub fn unique(&self) -> Self {
        Self(self.0.map(|id| id.unique()))
    }
}

impl From<Loc<Id>> for LocId {
    fn from(id: Loc<Id>) -> Self {
        Self(id)
    }
}

impl<S> From<S> for LocId
where
    S: ToString,
{
    fn from(s: S) -> Self {
        Self::new(Id::new(s), Span::default())
    }
}

impl Spanned for LocId {
    fn span(&self) -> Span {
        *self.0.tag()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct MetaId(Tagged<Id, Meta>);

impl MetaId {
    pub fn new(id: Id, meta: Meta) -> Self {
        Self(Tagged::new(id, meta))
    }

    pub fn id(&self) -> &Id {
        self.0.value()
    }

    pub fn into_id(&self) -> Id {
        *self.id()
    }

    pub fn span(&self) -> Span {
        self.0.tag().span()
    }

    pub fn unique(&self) -> Self {
        Self(self.0.map(|id| id.unique()))
    }

    pub fn rename(&mut self, id: &Id) {
        self.0 = self.0.map(|_| *id)
    }
}

impl From<&LocId> for MetaId {
    fn from(id: &LocId) -> Self {
        let inner = id.0.map_tag(Meta::new);
        Self(inner)
    }
}

impl From<LocId> for MetaId {
    fn from(id: LocId) -> Self {
        Self::from(&id)
    }
}

impl<S> From<S> for MetaId
where
    S: ToString,
{
    fn from(s: S) -> Self {
        Self::new(Id::new(s), Meta::default())
    }
}

impl Spanned for MetaId {
    fn span(&self) -> Span {
        self.0.tag().span()
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Serialize)]
pub struct Meta {
    pub size: Option<usize>,
    pub hint: Option<Hint>,
    #[serde(skip_serializing)]
    pub span: Span,
}

impl Meta {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            ..Default::default()
        }
    }
}

impl Spanned for Meta {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum Hint {
    Unsigned,
    Signed,
    Float,
}
