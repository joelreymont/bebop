use bebop_parser::error::*;
use bebop_util::meta::*;
use std::fmt;

#[derive(PartialEq, Eq, Clone)]
pub enum LiftError {
    Unknown(Span),
    Invalid(Span),
    Duplicate(Span),
    TypeMismatch(Span),
    InternalTypeMismatch(Span),
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
            Self::InternalTypeMismatch(span) => {
                write!(f, "Internal type mismatch {span:?}")
            }
            Self::ParserError(e) => write!(f, "Parser error {e:?}"),
        }
    }
}

impl Spanned for LiftError {
    fn span(&self) -> &Span {
        match self {
            Self::Unknown(span) => span,
            Self::Invalid(span) => span,
            Self::Duplicate(span) => span,
            Self::TypeMismatch(span) => span,
            Self::InternalTypeMismatch(span) => span,
            Self::ParserError(e) => e.span(),
        }
    }
}
