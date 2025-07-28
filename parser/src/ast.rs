use crate::error::*;
use bebop_util::{id::*, meta::*};
use lalrpop_util::ParseError;
use lazy_static::lazy_static;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt;

enum Special {
    Big,
    Little,
    Size,
    WordSize,
    Offset,
    IsDefault,
    RomSpace,
    RamSpace,
    RegisterSpace,
}

lazy_static! {
    static ref SPECIAL: HashMap<Id, Special> = {
        let mut m = HashMap::new();
        use Special::*;
        m.insert(Id::new("big"), Big);
        m.insert(Id::new("little"), Little);
        m.insert(Id::new("size"), Size);
        m.insert(Id::new("wordsize"), WordSize);
        m.insert(Id::new("default"), IsDefault);
        m.insert(Id::new("offset"), Offset);
        m.insert(Id::new("rom_space"), RomSpace);
        m.insert(Id::new("ram_space"), RamSpace);
        m.insert(Id::new("register_space"), RegisterSpace);
        m
    };
}

#[derive(Clone, PartialEq, Serialize)]
pub enum Definition {
    Endian(Loc<Endian>),
    Alignment(Loc<usize>),
    Token(Token),
    Space(Space),
    Varnode(Loc<Varnode>),
    PCodeOp(LocId),
    VarnodeAttach(Loc<VarnodeAttach>),
    Constructor(Constructor),
    Macro(Macro),
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum Endian {
    Big,
    Little,
}

impl fmt::Display for Endian {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Big => write!(f, "big"),
            Self::Little => write!(f, "little"),
        }
    }
}

impl Endian {
    pub fn parse<'a>(id: LocId) -> Result<Endian, LalrParseError<'a>> {
        let span = id.span();
        let id = Id::new(id.id().to_lowercase());
        let error = ParseError::User {
            error: ParserError::InvalidToken(span),
        };
        match SPECIAL.get(&id) {
            Some(value) => {
                use Special::*;
                match value {
                    Big => Ok(Endian::Big),
                    Little => Ok(Endian::Little),
                    _ => Err(error),
                }
            }
            None => Err(error),
        }
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Token {
    pub id: LocId,
    pub bit_width: Loc<usize>,
    pub fields: Vec<Field>,
}

#[derive(Clone, PartialEq, Serialize)]
pub enum FieldMod {
    IsSigned,
    IsHex,
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Field {
    pub id: LocId,
    pub start_bit: Loc<usize>,
    pub end_bit: Loc<usize>,
    pub is_signed: bool,
    pub is_hex: bool,
}

impl Field {
    pub fn new(
        id: LocId,
        start_bit: Loc<usize>,
        end_bit: Loc<usize>,
        mods: Vec<FieldMod>,
    ) -> Self {
        let this = Self {
            id,
            start_bit,
            end_bit,
            is_signed: false,
            is_hex: false,
        };
        mods.into_iter().fold(this, |mut this, m| {
            match m {
                FieldMod::IsSigned => this.is_signed = true,
                FieldMod::IsHex => this.is_hex = true,
            }
            this
        })
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub enum SpaceMod {
    Size(Loc<usize>),
    WordSize(Loc<usize>),
    IsDefault,
}

impl SpaceMod {
    pub fn parse<'a>(
        id: LocId,
        num: Option<Loc<usize>>,
    ) -> Result<SpaceMod, LalrParseError<'a>> {
        let span = id.span();
        let id = Id::new(id.id().to_lowercase());
        let error = ParseError::User {
            error: ParserError::InvalidToken(span),
        };
        use Special::*;
        match SPECIAL.get(&id) {
            Some(kind) => match kind {
                Size => Ok(SpaceMod::Size(num.unwrap())),
                WordSize => Ok(SpaceMod::WordSize(num.unwrap())),
                IsDefault => Ok(SpaceMod::IsDefault),
                _ => Err(error),
            },
            None => Err(error),
        }
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Space {
    pub id: LocId,
    pub kind: SpaceKind,
    pub size: Loc<usize>,
    pub word_size: Loc<usize>,
    pub is_default: bool,
}

impl Space {
    pub fn new(id: LocId, kind: SpaceKind, mods: Vec<SpaceMod>) -> Self {
        let this = Self {
            id,
            kind,
            size: Loc::new(0, Span::default()),
            word_size: Loc::new(1, Span::default()),
            is_default: true,
        };
        mods.into_iter().fold(this, |mut this, m| {
            match m {
                SpaceMod::Size(n) => this.size = n,
                SpaceMod::WordSize(n) => this.word_size = n,
                SpaceMod::IsDefault => this.is_default = true,
            }
            this
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum SpaceKind {
    Rom,
    Ram,
    Register,
}

impl SpaceKind {
    pub fn parse<'a>(id: LocId) -> Result<SpaceKind, LalrParseError<'a>> {
        let span = id.span();
        let id = Id::new(id.id().to_lowercase());
        let error = ParseError::User {
            error: ParserError::InvalidToken(span),
        };
        use Special::*;
        match SPECIAL.get(&id) {
            Some(kind) => match kind {
                RomSpace => Ok(SpaceKind::Rom),
                RamSpace => Ok(SpaceKind::Ram),
                RegisterSpace => Ok(SpaceKind::Register),
                _ => Err(error),
            },
            None => Err(error),
        }
    }
}

#[derive(Clone, PartialEq, Serialize)]

pub enum VarnodeMod {
    ByteSize(Loc<usize>),
    Offset(Loc<usize>),
}

impl VarnodeMod {
    pub fn parse<'a>(
        id: LocId,
        num: Loc<usize>,
    ) -> Result<VarnodeMod, LalrParseError<'a>> {
        let span = id.span();
        let id = Id::new(id.id().to_lowercase());
        let error = ParseError::User {
            error: ParserError::InvalidToken(span),
        };
        use Special::*;
        match SPECIAL.get(&id) {
            Some(kind) => match kind {
                Size => Ok(VarnodeMod::ByteSize(num)),
                Offset => Ok(VarnodeMod::Offset(num)),
                _ => Err(error),
            },
            None => Err(error),
        }
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Varnode {
    pub offset: Loc<usize>,
    pub byte_size: Loc<usize>,
    pub ids: Vec<LocId>,
}

impl Varnode {
    pub fn new(ids: Vec<LocId>, mods: Vec<VarnodeMod>) -> Self {
        let this = Self {
            ids,
            byte_size: Loc::new(0, Span::default()),
            offset: Loc::new(0, Span::default()),
        };
        mods.into_iter().fold(this, |mut this, m| {
            match m {
                VarnodeMod::ByteSize(n) => this.byte_size = n,
                VarnodeMod::Offset(n) => this.offset = n,
            }
            this
        })
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub struct VarnodeAttach {
    pub fields: Vec<LocId>,
    pub registers: Vec<LocId>,
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Constructor {
    pub id: LocId,
    pub display: Display,
    pub pattern: Option<Expr>,
    pub context: Vec<Statement>,
    pub body: Vec<Statement>,
    pub is_instruction: bool,
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Display {
    pub mnemonic: Vec<DisplayPiece>,
    pub output: Vec<DisplayPiece>,
}

#[derive(Clone, PartialEq, Serialize)]
pub enum DisplayPiece {
    Id(LocId),
    Text(Id),
    Caret,
    Space,
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Macro {
    pub id: LocId,
    pub args: Vec<LocId>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expr {
    Binary {
        op: Loc<BinaryOp>,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: Loc<UnaryOp>,
        rhs: Box<Expr>,
    },
    Paren(Box<Expr>),
    FunCall {
        id: LocId,
        args: Vec<Box<Expr>>,
    },
    BitRange {
        id: LocId,
        start_bit: Loc<usize>,
        bit_width: Loc<usize>,
    },
    Sized {
        expr: Box<Expr>,
        size: Loc<usize>,
    },
    Pointer {
        expr: Box<Expr>,
        space: Option<LocId>,
    },
    Id(LocId),
    Int(Loc<usize>),
    Unit(Loc<()>),
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        use Expr::*;
        match self {
            Binary { lhs, .. } => lhs.span(),
            Unary { rhs, .. } => rhs.span(),
            Paren(expr) => expr.span(),
            FunCall { id, .. } => id.span(),
            BitRange { id, .. } => id.span(),
            Sized { expr, .. } => expr.span(),
            Pointer { expr, .. } => expr.span(),
            Id(id) => id.span(),
            Int(n) => n.span(),
            Unit(x) => x.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Statement {
    Bind { lhs: Expr, rhs: Expr },
    Goto(JumpTarget),
    Call(JumpTarget),
    Return(JumpTarget),
    Branch { condition: Expr, target: JumpTarget },
    FunCall { id: LocId, args: Vec<Expr> },
    Export(Expr),
    Label(LocId),
    Build(LocId),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum JumpTarget {
    Fixed {
        address: Loc<usize>,
        space: Option<LocId>,
    },
    Direct(LocId),
    Indirect(Expr),
    Label(LocId),
}

impl Spanned for JumpTarget {
    fn span(&self) -> Span {
        use JumpTarget::*;
        match self {
            Fixed { address, .. } => address.span(),
            Direct(id) => id.span(),
            Indirect(expr) => expr.span(),
            Label(id) => id.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum UnaryOp {
    NOT,
    NEG,
    FNEG,
    INV,
    AlignLeft,
    AlignRight,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NEG => write!(f, "-"),
            Self::FNEG => write!(f, "f-"),
            Self::NOT => write!(f, "!"),
            Self::INV => write!(f, "~"),
            Self::AlignLeft | Self::AlignRight => write!(f, "..."),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum BinaryOp {
    OR,
    AND,
    XOR,
    LOR,
    LAND,
    LXOR,
    EQ,
    NE,
    FEQ,
    FNE,
    LT,
    GT,
    LE,
    GE,
    SLT,
    SGT,
    SLE,
    SGE,
    FLT,
    FGT,
    FLE,
    FGE,
    LSHIFT,
    RSHIFT,
    SLSHIFT,
    SRSHIFT,
    PLUS,
    MINUS,
    FPLUS,
    FMINUS,
    MUL,
    DIV,
    MOD,
    SDIV,
    SMOD,
    FMUL,
    FDIV,
    JOIN,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::OR => write!(f, "|"),
            Self::AND => write!(f, "&"),
            Self::XOR => write!(f, "^"),
            Self::LOR => write!(f, "||"),
            Self::LAND => write!(f, "&&"),
            Self::LXOR => write!(f, "^^"),
            Self::EQ => write!(f, "=="),
            Self::NE => write!(f, "!="),
            Self::FEQ => write!(f, "f=="),
            Self::FNE => write!(f, "f!="),
            Self::LT => write!(f, "<"),
            Self::GT => write!(f, ">"),
            Self::LE => write!(f, "<="),
            Self::GE => write!(f, ">="),
            Self::SLT => write!(f, "s<"),
            Self::SGT => write!(f, "s>"),
            Self::SLE => write!(f, "s<="),
            Self::SGE => write!(f, "s>="),
            Self::FLT => write!(f, "f<"),
            Self::FGT => write!(f, "f>"),
            Self::FLE => write!(f, "f<="),
            Self::FGE => write!(f, "f>="),
            Self::LSHIFT => write!(f, "<<"),
            Self::RSHIFT => write!(f, ">>"),
            Self::SLSHIFT => write!(f, "s<<"),
            Self::SRSHIFT => write!(f, "s>>"),
            Self::PLUS => write!(f, "+"),
            Self::MINUS => write!(f, "-"),
            Self::FPLUS => write!(f, "f+"),
            Self::FMINUS => write!(f, "f-"),
            Self::MUL => write!(f, "*"),
            Self::DIV => write!(f, "/"),
            Self::MOD => write!(f, "%"),
            Self::SDIV => write!(f, "s/"),
            Self::SMOD => write!(f, "s%"),
            Self::FMUL => write!(f, "f*"),
            Self::FDIV => write!(f, "f/"),
            Self::JOIN => write!(f, ";"),
        }
    }
}
