use crate::hir;
use bebop_parser::error::*;
use bebop_util::meta::*;
use std::fmt;

#[derive(Clone)]
pub enum LiftError {
    Unknown(Span),
    Invalid(Span),
    Duplicate(Span),
    TypeMismatch(Span),
    SizeMismatch { span: Span, want: usize, got: usize },
    InternalTypeMismatch { ty: hir::Type },
    ParserError(ParserError),
}

impl fmt::Debug for LiftError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unknown(span) => write!(f, "Unknown {span:?}"),
            Self::Invalid(span) => write!(f, "Invalid {span:?}"),
            Self::Duplicate(span) => write!(f, "Duplicate {span:?}"),
            Self::TypeMismatch(span) => {
                write!(f, "Type mismatch {span:?}")
            }
            Self::SizeMismatch { want, got, .. } => {
                write!(f, "Size mismatch: want {want} but got {got}")
            }
            Self::InternalTypeMismatch { ty, .. } => {
                write!(f, "Internal type mismatch {ty:?}")
            }
            Self::ParserError(e) => write!(f, "Parser error {e:?}"),
        }
    }
}

impl Spanned for LiftError {
    fn span(&self) -> Span {
        match self {
            Self::Unknown(span) => *span,
            Self::Invalid(span) => *span,
            Self::Duplicate(span) => *span,
            Self::TypeMismatch(span) => *span,
            Self::SizeMismatch { span, .. } => *span,
            Self::InternalTypeMismatch { ty } => ty.span(),
            Self::ParserError(e) => e.span(),
        }
    }
}
