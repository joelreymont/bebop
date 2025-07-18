use crate::error::*;
use internment::Intern;
use lalrpop_util::ParseError;
use lazy_static::lazy_static;
use serde::{Serialize, Serializer};
use std::collections::HashMap;
use std::{fmt, ops::Deref};

use bebop_util::meta::*;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ident(Intern<String>);

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "`{}`", self.0)
    }
}

impl Ident {
    pub fn new<S: ToString>(s: S) -> Self {
        Self(Intern::new(s.to_string()))
    }
}

impl Deref for Ident {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<String> for Ident {
    fn as_ref(&self) -> &'static String {
        self.0.as_ref()
    }
}

impl Serialize for Ident {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = self.0;
        s.serialize(serializer)
    }
}

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
    static ref SPECIAL: HashMap<Ident, Special> = {
        let mut m = HashMap::new();
        use Special::*;
        m.insert(Ident::new("big"), Big);
        m.insert(Ident::new("little"), Little);
        m.insert(Ident::new("size"), Size);
        m.insert(Ident::new("wordsize"), WordSize);
        m.insert(Ident::new("default"), IsDefault);
        m.insert(Ident::new("offset"), Offset);
        m.insert(Ident::new("rom_space"), RomSpace);
        m.insert(Ident::new("ram_space"), RamSpace);
        m.insert(Ident::new("register_space"), RegisterSpace);
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
    PCodeOp(Loc<Ident>),
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
    pub fn parse<'a>(
        id: Loc<Ident>,
    ) -> Result<Endian, LalrParseError<'a>> {
        let span = id.span();
        let id = Ident::new(id.value().to_lowercase());
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
    pub id: Loc<Ident>,
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
    pub id: Loc<Ident>,
    pub start_bit: Loc<usize>,
    pub end_bit: Loc<usize>,
    pub is_signed: bool,
    pub is_hex: bool,
}

impl Field {
    pub fn new(
        id: Loc<Ident>,
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
        id: Loc<Ident>,
        num: Option<Loc<usize>>,
    ) -> Result<SpaceMod, LalrParseError<'a>> {
        let span = id.span();
        let id = Ident::new(id.value().to_lowercase());
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
    pub id: Loc<Ident>,
    pub kind: SpaceKind,
    pub size: Loc<usize>,
    pub word_size: Loc<usize>,
    pub is_default: bool,
}

impl Space {
    pub fn new(
        id: Loc<Ident>,
        kind: SpaceKind,
        mods: Vec<SpaceMod>,
    ) -> Self {
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
    pub fn parse<'a>(
        id: Loc<Ident>,
    ) -> Result<SpaceKind, LalrParseError<'a>> {
        let span = id.span();
        let id = Ident::new(id.value().to_lowercase());
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
        id: Loc<Ident>,
        num: Loc<usize>,
    ) -> Result<VarnodeMod, LalrParseError<'a>> {
        let span = id.span();
        let id = Ident::new(id.value().to_lowercase());
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
    pub ids: Vec<Loc<Ident>>,
}

impl Varnode {
    pub fn new(ids: Vec<Loc<Ident>>, mods: Vec<VarnodeMod>) -> Self {
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
    pub fields: Vec<Loc<Ident>>,
    pub registers: Vec<Loc<Ident>>,
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Constructor {
    pub id: Loc<Ident>,
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
    Id(Loc<Ident>),
    Text(Ident),
    Caret,
    Space,
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Macro {
    pub id: Loc<Ident>,
    pub args: Vec<Loc<Ident>>,
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
        id: Loc<Ident>,
        args: Vec<Box<Expr>>,
    },
    BitRange {
        id: Loc<Ident>,
        start_bit: Loc<usize>,
        bit_width: Loc<usize>,
    },
    Sized {
        expr: Box<Expr>,
        size: Loc<usize>,
    },
    Pointer {
        expr: Box<Expr>,
        space: Option<Loc<Ident>>,
    },
    Id(Loc<Ident>),
    Int(Loc<usize>),
    Unit(Loc<()>),
}

impl Expr {
    pub fn span(&self) -> Span {
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
    FunCall { id: Loc<Ident>, args: Vec<Expr> },
    Export(Expr),
    Label(Loc<Ident>),
    Build(Loc<Ident>),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum JumpTarget {
    Fixed {
        address: Loc<usize>,
        space: Option<Loc<Ident>>,
    },
    Direct(Loc<Ident>),
    Indirect(Expr),
    Label(Loc<Ident>),
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
