use serde::{Serialize, Serializer};
use std::{
    cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd},
    fmt,
    ops::{Deref, DerefMut, Range},
};

#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Range<usize> {
        value.start..value.end
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Span {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

#[derive(Copy, Clone)]
pub struct Tagged<V, T = ()> {
    value: V,
    tag: T,
}

impl<V: Serialize, T> Serialize for Tagged<V, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.value.serialize(serializer)
    }
}

impl<V, T> Tagged<V, T> {
    pub fn new(value: V, tag: T) -> Self {
        Tagged { value, tag }
    }

    pub fn value(&self) -> &V {
        &self.value
    }

    pub fn value_mut(&mut self) -> &mut V {
        &mut self.value
    }

    pub fn into_value(self) -> V {
        self.value
    }

    pub fn map<U, F: FnOnce(V) -> U>(self, f: F) -> Tagged<U, T> {
        Tagged {
            value: f(self.value),
            tag: self.tag,
        }
    }

    pub fn map_tag<U, F: FnOnce(T) -> U>(self, f: F) -> Tagged<V, U> {
        Tagged {
            value: self.value,
            tag: f(self.tag),
        }
    }

    pub fn tag(&self) -> &T {
        &self.tag
    }

    pub fn tag_mut(&mut self) -> &mut T {
        &mut self.tag
    }

    pub fn split(self) -> (V, T) {
        (self.value, self.tag)
    }

    pub fn as_mut(&mut self) -> (&mut V, &mut T) {
        (&mut self.value, &mut self.tag)
    }
}

impl<V, T> Deref for Tagged<V, T> {
    type Target = V;
    fn deref(&self) -> &Self::Target {
        self.value()
    }
}

impl<V, T> DerefMut for Tagged<V, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.value_mut()
    }
}

impl<V: PartialEq, T> PartialEq for Tagged<V, T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<V: Eq, T> Eq for Tagged<V, T> {}

impl<V: Ord, T> Ord for Tagged<V, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
    }
}

impl<V: PartialOrd, T> PartialOrd for Tagged<V, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<V: fmt::Debug, T: fmt::Debug> fmt::Debug for Tagged<V, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?} @ {:?}", self.value, self.tag)
    }
}

pub type Loc<V> = Tagged<V, Span>;

impl<V> Loc<V> {
    pub fn span(&self) -> Span {
        self.tag
    }
}
