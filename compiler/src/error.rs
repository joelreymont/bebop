use crate::hir::*;
use bebop_parser::error::*;
use bebop_util::meta::*;
use std::fmt;

#[derive(Clone)]
pub enum TypeEnvError {
    TooManyEntries(ExprPtr),
}

#[derive(Clone)]
pub enum LiftError {
    Unknown(Id),
    Invalid(Span),
    Duplicate(Span),
    TypeMismatch(Span),
    SizeMismatch { span: Span, want: usize, got: usize },
    InternalTypeMismatch(Span),
    ParserError(ParserError),
}

impl fmt::Debug for LiftError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unknown(id) => write!(f, "Unknown {id:?}"),
            Self::Invalid(span) => write!(f, "Invalid {span:?}"),
            Self::Duplicate(span) => write!(f, "Duplicate {span:?}"),
            Self::TypeMismatch(span) => {
                write!(f, "Type mismatch {span:?}")
            }
            Self::SizeMismatch { want, got, .. } => {
                write!(f, "Size mismatch: want {want} but got {got}")
            }
            Self::InternalTypeMismatch { .. } => {
                write!(f, "Internal type mismatch")
            }
            Self::ParserError(e) => write!(f, "Parser error {e:?}"),
        }
    }
}

impl Spanned for LiftError {
    fn span(&self) -> Span {
        match self {
            Self::Unknown(id) => id.span(),
            Self::Invalid(span) => *span,
            Self::Duplicate(span) => *span,
            Self::TypeMismatch(span) => *span,
            Self::SizeMismatch { span, .. } => *span,
            Self::InternalTypeMismatch(span) => *span,
            Self::ParserError(e) => e.span(),
        }
    }
}
