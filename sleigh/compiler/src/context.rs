use bebop_sleigh_parser::{ast, meta::*};
use serde::Serialize;
use std::collections::HashMap;

type Ident = ast::Ident;

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Tag {
    pub size: Option<usize>,
    pub hint: Hint,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum Hint {
    Unsigned,
    Signed,
    Float,
}

pub struct Context {
    pub endian: ast::Endian,
    pub alignment: u8,
    pub type_env: TypeEnv,
    pub default_region: Option<Ident>,
    scanners: Vec<Scanner>,
    register_maps: Vec<RegisterMap>,
}

type TypeEnv = HashMap<Ident, Type>;

pub enum Type {
    MemoryRegion(MemoryRegion),
    Register(Register),
    RegisterIndex(RegisterIndex),
    BitField(BitField),
    Intrinsic(Intrinsic),
    Macro(Macro),
    PCodeOp(PCodeOp),
    Scanner(Scanner),
    Variable(Variable),
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum Expr {
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        rhs: Box<Expr>,
    },
    Paren(Box<Expr>),
    Pointer {
        expr: Box<Expr>,
        region: Option<Ident>,
    },
    TakeBits {
        expr: Box<Expr>,
        start_bit: u8,
        bit_width: u8,
    },
    // Take_bytes of t * int * bytes_from * int (* n, from, bit width *)
    FunCall {
        intrisic: Ident,
        args: Vec<Box<Expr>>,
    },
    Register(Ident),
    RegisterIndex(Ident),
    BitField(Ident),
    Scanner(Ident),
    Variable(Ident),
    Int(usize),
    Unit,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum BinaryOp {
    BOR,
    BAND,
    BXOR,
    XOR,
    EQ,
    NE,
    GT,
    LT,
    LE,
    GE,
    LSHIFT,
    RSHIFT,
    PLUS,
    MINUS,
    DIV,
    MOD,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum UnaryOp {
    NOT,
    INV,
    NEG,
    FNEG,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Id {
    ident: Ident,
    tag: Tag,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct MemoryRegion {
    pub id: Id,
    pub kind: ast::SpaceKind,
    pub size: usize,
    pub word_size: usize,
    pub is_default: bool,
    pub tag: Tag,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Register {
    id: Ident,
    tag: Tag,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct RegisterMap {
    registers: Vec<Option<Ident>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct BitField {
    id: Ident,
    start_bit: u8,
    end_bit: u8,
    tag: Tag,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct RegisterIndex {
    bit_field: Ident,
    register_map: Ident,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Variable {
    id: Ident,
    tag: Tag,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Intrinsic {
    id: Ident,
    tag: Tag,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum JumpTarget {
    Fixed(Address),
    Direct(Loc<Ident>),
    Indirect(Box<Expr>),
    Label(Loc<Ident>),
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Address {
    pub address: usize,
    pub region: Option<Ident>,
}
