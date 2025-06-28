use crate::meta::*;
use internment::Intern;
use serde::Serialize;
use std::{fmt, ops::Deref};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
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
    pub fn as_ref(self) -> &'static String {
        self.0.as_ref()
    }
}

impl Deref for Ident {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct AST {
    pub pool: ExprPool,
    pub definitions: Vec<Definition>,
}

impl AST {
    pub fn new() -> Self {
        Self {
            pool: ExprPool::default(),
            definitions: Vec::new(),
        }
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub enum Definition {
    Endian(Loc<Endian>),
    Alignment(Loc<usize>),
    Token(Loc<Token>),
    Space(Loc<Space>),
    Varnode(Loc<Varnode>),
    PCodeOp(Loc<Ident>),
    VarnodeAttach(Loc<VarnodeAttach>),
    Constructor(Loc<Constructor>),
    Macro(Loc<Macro>),
}

#[derive(Copy, Clone, PartialEq, Serialize)]
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

#[derive(Clone, PartialEq, Serialize)]
pub struct Token {
    pub id: Loc<Ident>,
    pub bit_width: usize,
    pub fields: Vec<Field>,
}

#[derive(Copy, Clone, PartialEq, Serialize)]
pub enum FieldMod {
    IsSigned,
    IsHex,
}

#[derive(Copy, Clone, PartialEq, Serialize)]
pub struct Field {
    pub id: Loc<Ident>,
    pub start_bit: usize,
    pub end_bit: usize,
    pub is_signed: bool,
    pub is_hex: bool,
}

impl Field {
    pub fn new(id: Loc<Ident>, start_bit: usize, end_bit: usize, mods: Vec<FieldMod>) -> Self {
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

#[derive(Copy, Clone, PartialEq, Serialize)]
pub enum SpaceMod {
    Size(usize),
    WordSize(usize),
    IsDefault,
}

#[derive(Copy, Clone, PartialEq, Serialize)]
pub struct Space {
    pub id: Loc<Ident>,
    pub kind: SpaceKind,
    pub size: usize,
    pub word_size: usize,
    pub is_default: bool,
}

impl Space {
    pub fn new(id: Loc<Ident>, kind: SpaceKind, mods: Vec<SpaceMod>) -> Self {
        let this = Self {
            id,
            kind,
            size: 0,
            word_size: 1,
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

#[derive(Copy, Clone, PartialEq, Serialize)]
pub enum SpaceKind {
    Rom,
    Ram,
    Register,
}

#[derive(Copy, Clone, PartialEq, Serialize)]
pub enum VarnodeMod {
    Size(usize),
    Offset(usize),
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Varnode {
    pub offset: usize,
    pub size: usize,
    pub ids: Vec<Loc<Ident>>,
}

impl Varnode {
    pub fn new(ids: Vec<Loc<Ident>>, mods: Vec<VarnodeMod>) -> Self {
        let this = Self {
            ids,
            size: 0,
            offset: 0,
        };
        mods.into_iter().fold(this, |mut this, m| {
            match m {
                VarnodeMod::Size(n) => this.size = n,
                VarnodeMod::Offset(n) => this.offset = n,
            }
            this
        })
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub struct VarnodeAttach {
    pub fields: Vec<Loc<Ident>>,
    pub registers: Vec<Option<Loc<Ident>>>,
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Constructor {
    id: Ident,
    mnemonic: Vec<DisplayPiece>,
    output: Vec<DisplayPiece>,
    pattern: Option<Expr>,
    context: Option<ConstructorContext>,
    body: Vec<ExprRef>,
}

#[derive(Copy, Clone, PartialEq, Serialize)]
pub enum DisplayPiece {
    Id(Loc<Ident>),
    Text(Ident),
}

#[derive(Clone, PartialEq, Serialize)]
pub struct ConstructorContext {
    body: Vec<ExprRef>,
}

#[derive(Clone, PartialEq, Serialize)]
pub struct Macro {
    id: Loc<Ident>,
    args: Vec<Loc<Ident>>,
    body: Vec<ExprRef>,
}

#[derive(Debug, Clone, PartialEq, Copy, Serialize)]
pub struct ExprRef(usize);

pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    fn default() -> Self {
        Self(Vec::with_capacity(100_000_000))
    }

    pub fn get(&self, expr: ExprRef) -> &Expr {
        &self.0[expr.0 as usize]
    }

    pub fn add(&mut self, expr: Expr) -> ExprRef {
        let ix = self.0.len();
        self.0.push(expr);
        ExprRef(ix)
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub enum Expr {
    Binary {
        op: Loc<BinaryOp>,
        lhs: ExprRef,
        rhs: ExprRef,
    },
    Unary {
        op: UnaryOp,
        rhs: ExprRef,
    },
    Paren(ExprRef),
    FunCall {
        id: Loc<Ident>,
        args: Vec<ExprRef>,
    },
    Id(Loc<Ident>),
    Int(Loc<usize>),
    BitRange {
        id: Loc<Ident>,
        start_bit: Loc<usize>,
        width: Loc<usize>,
    },
    Sized {
        expr: ExprRef,
        size: Loc<usize>,
    },
    Pointer {
        expr: ExprRef,
        space: Option<Loc<Ident>>,
    },
    Bind {
        lhs: ExprRef,
        rhs: ExprRef,
    },
    Build(Loc<Ident>),
    Goto(JumpTarget),
    Call(JumpTarget),
    Return(JumpTarget),
    Branch {
        condition: ExprRef,
        target: JumpTarget,
    },
    Export(ExprRef),
    Label(Loc<Ident>),
    Nothing,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum JumpTarget {
    Fixed {
        address: Loc<usize>,
        space: Option<Loc<Ident>>,
    },
    Direct(Loc<Ident>),
    Indirect(ExprRef),
    Label(Loc<Ident>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize)]
pub enum UnaryOp {
    Not,
    Neg,
    Inv,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
            Self::Inv => write!(f, "~"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum BinaryOp {
    Or,
    And,
    Xor,
    LogOr,
    LogAnd,
    LogXor,
    Equal,
    NotEqual,
    FloatEqual,
    FloatNotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    SignedLess,
    SignedGreater,
    SignedLessEqual,
    SignedGreaterEqual,
    FloatLess,
    FloatGreater,
    FloatLessEqual,
    FloatGreaterEqual,
    ShiftLeft,
    ShiftRight,
    SignedShiftLeft,
    SignedShiftRight,
    Plus,
    Minus,
    FloatPlus,
    FloatMinus,
    Mul,
    Div,
    Mod,
    SignedDiv,
    SignedMod,
    FloatMul,
    FloatDiv,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Or => write!(f, "|"),
            Self::And => write!(f, "&"),
            Self::Xor => write!(f, "^"),
            Self::LogOr => write!(f, "||"),
            Self::LogAnd => write!(f, "&&"),
            Self::LogXor => write!(f, "^^"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::FloatEqual => write!(f, "f=="),
            Self::FloatNotEqual => write!(f, "f!="),
            Self::Less => write!(f, "<"),
            Self::Greater => write!(f, ">"),
            Self::LessEqual => write!(f, "<="),
            Self::GreaterEqual => write!(f, ">="),
            Self::SignedLess => write!(f, "s<"),
            Self::SignedGreater => write!(f, "s>"),
            Self::SignedLessEqual => write!(f, "s<="),
            Self::SignedGreaterEqual => write!(f, "s>="),
            Self::FloatLess => write!(f, "f<"),
            Self::FloatGreater => write!(f, "f>"),
            Self::FloatLessEqual => write!(f, "f<="),
            Self::FloatGreaterEqual => write!(f, "f>="),
            Self::ShiftLeft => write!(f, "<<"),
            Self::ShiftRight => write!(f, ">>"),
            Self::SignedShiftLeft => write!(f, "s<<"),
            Self::SignedShiftRight => write!(f, "s>>"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::FloatPlus => write!(f, "f+"),
            Self::FloatMinus => write!(f, "f-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::SignedDiv => write!(f, "s/"),
            Self::SignedMod => write!(f, "s%"),
            Self::FloatMul => write!(f, "f*"),
            Self::FloatDiv => write!(f, "f/"),
        }
    }
}
