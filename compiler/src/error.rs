use bebop_parser::error::ParserError;
use bebop_util::id::MetaId;
use bebop_util::meta::{Span, Spanned};
use std::fmt;

#[derive(Clone)]
pub enum Error {
    ParserError(ParserError),
    Unknown(MetaId),
    Invalid(Span),
    Duplicate(Span),
    TypeMismatch(Span),
    SizeMismatch { span: Span, want: usize, got: usize },
    MacroArgumentMismatch(Span),
    InternalTypeMismatch(Span),
    MacroScopeMerge(Span),
    Rename(Span),
    VariableLengthPattern(Span),
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ParserError(e) => write!(f, "Parser error {e:?}"),
            Self::Unknown(id) => write!(f, "Unknown {id:?}"),
            Self::Invalid(span) => write!(f, "Invalid {span:?}"),
            Self::Duplicate(span) => write!(f, "Duplicate {span:?}"),
            Self::TypeMismatch(span) => {
                write!(f, "Type mismatch {span:?}")
            }
            Self::SizeMismatch { want, got, .. } => {
                write!(f, "Size mismatch: want {want} but got {got}")
            }
            Self::InternalTypeMismatch(span) => {
                write!(f, "Internal type mismatch: {span:?}")
            }
            Self::MacroArgumentMismatch(span) => {
                write!(f, "Macro argument mismatch: {span:?}")
            }
            Self::MacroScopeMerge(span) => {
                write!(
                    f,
                    "Duplicate scope item found while expanding macro: {span:?}"
                )
            }
            Self::Rename(span) => {
                write!(f, "Internal rename error: {span:?}")
            }
            Self::VariableLengthPattern(span) => {
                write!(
                    f,
                    "Variable-length instructions unsupported: {span:?}"
                )
            }
        }
    }
}

impl Spanned for Error {
    fn span(&self) -> Span {
        match self {
            Self::ParserError(e) => e.span(),
            Self::Unknown(id) => id.span(),
            Self::Invalid(span) => *span,
            Self::Duplicate(span) => *span,
            Self::TypeMismatch(span) => *span,
            Self::SizeMismatch { span, .. } => *span,
            Self::InternalTypeMismatch(span) => *span,
            Self::MacroArgumentMismatch(span) => *span,
            Self::MacroScopeMerge(span) => *span,
            Self::Rename(span) => *span,
            Self::VariableLengthPattern(span) => *span,
        }
    }
}
