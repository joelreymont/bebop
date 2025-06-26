use internment::Intern;
use serde::Serialize;
use std::{
    cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd},
    fmt,
    ops::{Deref, DerefMut, Range},
    path::{Path, PathBuf},
};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct FileId(Intern<Vec<String>>);

impl fmt::Display for FileId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.len() == 0 {
            write!(f, "?")
        } else {
            write!(f, "{}", self.0.clone().join("/"))
        }
    }
}

impl fmt::Debug for FileId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl FileId {
    pub fn empty() -> Self {
        FileId(Intern::new(Vec::new()))
    }

    pub fn repl() -> Self {
        FileId(Intern::new(vec!["repl".to_string()]))
    }

    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        FileId(Intern::new(
            path.as_ref()
                .iter()
                .map(|c| c.to_string_lossy().into_owned())
                .collect(),
        ))
    }

    pub fn to_path(&self) -> PathBuf {
        self.0.iter().map(|e| e.to_string()).collect()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Serialize)]
pub struct Span {
    src: FileId,
    range: (usize, usize),
}

impl Span {
    pub fn empty() -> Self {
        Self::new(FileId::empty(), 0..0)
    }

    pub fn new(src: FileId, range: Range<usize>) -> Self {
        assert!(range.start <= range.end);
        Self {
            src,
            range: (range.start, range.end),
        }
    }

    pub fn src(&self) -> FileId {
        self.src
    }

    pub fn range(&self) -> Range<usize> {
        self.start()..self.end()
    }

    pub fn join(self, other: Self) -> Self {
        assert_eq!(self.src, other.src, "joining spans with different sources");
        Self {
            range: (self.start().min(other.start()), self.end().max(other.end())),
            ..self
        }
    }

    fn start(&self) -> usize {
        self.range.0
    }
    fn end(&self) -> usize {
        self.range.1
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.src, self.range())
    }
}

impl ariadne::Span for Span {
    type SourceId = FileId;

    fn source(&self) -> &FileId {
        &self.src
    }

    fn start(&self) -> usize {
        self.range.0
    }
    fn end(&self) -> usize {
        self.range.1
    }
}

#[derive(Copy, Clone, Serialize)]
pub struct Tagged<V, T = ()> {
    value: V,
    tag: T,
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

    pub fn tag(&self) -> &T {
        &self.tag
    }

    pub fn tag_mut(&mut self) -> &mut T {
        &mut self.tag
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
