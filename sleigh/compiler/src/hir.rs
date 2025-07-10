use crate::{env::Environment, error::LiftError};
use bebop_sleigh_parser::ast;
use bebop_sleigh_util::meta::*;
use serde::Serialize;
use std::{cell::RefCell, option::Option::*, rc::Rc};

pub type Ident = ast::Ident;
pub type Endian = ast::Endian;

pub type TypeEnv = Environment<Ident, Type>;

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Tag {
    pub size: Option<usize>,
    pub hint: Option<Hint>,
    #[serde(skip_serializing)]
    pub span: Span,
}

impl From<Span> for Tag {
    fn from(span: Span) -> Self {
        Self {
            span,
            size: None,
            hint: None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum Hint {
    Unsigned,
    Signed,
    Float,
}

type Ptr<T> = Rc<T>;
type PtrMut<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Type {
    MemoryRegion(Ptr<MemoryRegion>),
    Register(Ptr<Register>),
    RegisterIndex(Ptr<RegisterIndex>),
    BitField(Ptr<BitField>),
    Intrinsic(Ptr<Intrinsic>),
    Macro(Ptr<Macro>),
    PCodeOp(Ptr<PCodeOp>),
    Scanner(PtrMut<Scanner>),
    Variable(Ptr<Variable>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
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
        region: Option<Ptr<MemoryRegion>>,
    },
    TakeBits {
        id: Id,
        start_bit: usize,
        bit_width: usize,
    },
    TakeBytes {
        expr: Box<Expr>,
        n: usize,
    },
    FunCall {
        intrinsic: Ptr<Intrinsic>,
        args: Vec<Box<Expr>>,
    },
    Register(Ptr<Register>),
    RegisterIndex(Ptr<RegisterIndex>),
    BitField(Ptr<BitField>),
    Scanner(PtrMut<Scanner>),
    Variable(Ptr<Variable>),
    Int(usize),
    Unit,
}

impl Expr {
    pub fn lift(scope: &Scope, expr: ast::Expr) -> Result<Self, LiftError> {
        use ast::Expr::*;
        match expr {
            Binary { op, lhs, rhs } => {
                let lhs = Self::lift(scope, *lhs)?;
                let rhs = Self::lift(scope, *rhs)?;
                let op = BinaryOp::try_from(op)?;
                let expr = Expr::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                Ok(expr)
            }
            Unary { op, rhs } => {
                let rhs = Self::lift(scope, *rhs)?;
                let op = UnaryOp::try_from(op)?;
                let expr = Expr::Unary {
                    op,
                    rhs: Box::new(rhs),
                };
                Ok(expr)
            }
            Paren(expr) => {
                let expr = Self::lift(scope, *expr)?;
                Ok(Expr::Paren(Box::new(expr)))
            }
            FunCall { id, args } => {
                let intrinsic = scope.lookup(&id)?.try_into()?;
                let args: Result<Vec<Box<Expr>>, _> = args
                    .into_iter()
                    .map(|arg| Ok(Box::new(Self::lift(scope, *arg)?)))
                    .collect();
                Ok(Expr::FunCall {
                    intrinsic,
                    args: args?,
                })
            }
            BitRange {
                id,
                start_bit,
                bit_width,
            } => {
                let expr = Expr::TakeBits {
                    id: self::Id::from(id),
                    start_bit: start_bit.into_value(),
                    bit_width: bit_width.into_value(),
                };
                Ok(expr)
            }
            Sized { expr, size } => {
                let expr = Self::lift(scope, *expr)?;
                let expr = Expr::TakeBytes {
                    expr: Box::new(expr),
                    n: size.into_value(),
                };
                Ok(expr)
            }
            Pointer { expr, space } => {
                let expr = Self::lift(scope, *expr)?;
                let region = match space {
                    Some(id) => {
                        let region = scope.lookup(&id)?.try_into()?;
                        Some(region)
                    }
                    _ => None,
                };
                let expr = Expr::Pointer {
                    expr: Box::new(expr),
                    region,
                };
                Ok(expr)
            }
            Id(id) => scope.to_expr(&id),
            Int(n) => Ok(Expr::Int(n.into_value())),
            Unit(_) => Ok(Expr::Unit),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum BinaryOp {
    OR,
    AND,
    XOR,
    LOR,
    LAND,
    LXOR,
    EQ(Hint),
    NE(Hint),
    GT(Hint),
    LT(Hint),
    LE(Hint),
    GE(Hint),
    LSHIFT(Hint),
    RSHIFT(Hint),
    PLUS(Hint),
    MINUS(Hint),
    MUL(Hint),
    DIV(Hint),
    MOD(Hint),
}

impl TryFrom<Loc<ast::BinaryOp>> for BinaryOp {
    type Error = LiftError;

    fn try_from(op: Loc<ast::BinaryOp>) -> Result<Self, Self::Error> {
        let (op, span) = op.into();
        use ast::BinaryOp::*;
        let op = match op {
            OR => Self::OR,
            AND => Self::AND,
            XOR => Self::XOR,
            LOR => Self::LOR,
            LAND => Self::LAND,
            LXOR => Self::LXOR,
            EQ => Self::EQ(Hint::Unsigned),
            NE => Self::NE(Hint::Unsigned),
            FEQ => Self::EQ(Hint::Float),
            FNE => Self::NE(Hint::Float),
            LT => Self::LT(Hint::Unsigned),
            GT => Self::GT(Hint::Unsigned),
            LE => Self::LE(Hint::Unsigned),
            GE => Self::GE(Hint::Unsigned),
            SLT => Self::LT(Hint::Signed),
            SGT => Self::LT(Hint::Signed),
            SLE => Self::LE(Hint::Signed),
            SGE => Self::GE(Hint::Signed),
            FLT => Self::LT(Hint::Float),
            FGT => Self::GT(Hint::Float),
            FLE => Self::LE(Hint::Float),
            FGE => Self::GE(Hint::Float),
            LSHIFT => Self::LSHIFT(Hint::Unsigned),
            RSHIFT => Self::RSHIFT(Hint::Unsigned),
            SLSHIFT => Self::LSHIFT(Hint::Signed),
            SRSHIFT => Self::RSHIFT(Hint::Signed),
            PLUS => Self::PLUS(Hint::Unsigned),
            MINUS => Self::MINUS(Hint::Unsigned),
            FPLUS => Self::PLUS(Hint::Float),
            FMINUS => Self::MINUS(Hint::Float),
            MUL => Self::MUL(Hint::Unsigned),
            DIV => Self::DIV(Hint::Unsigned),
            MOD => Self::MOD(Hint::Unsigned),
            SDIV => Self::DIV(Hint::Signed),
            SMOD => Self::MOD(Hint::Signed),
            FMUL => Self::MUL(Hint::Float),
            FDIV => Self::DIV(Hint::Float),
            _ => return Err(LiftError::Invalid { span }),
        };
        Ok(op)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum UnaryOp {
    NOT,
    INV,
    NEG { is_float: bool },
}

impl TryFrom<Loc<ast::UnaryOp>> for UnaryOp {
    type Error = LiftError;

    fn try_from(op: Loc<ast::UnaryOp>) -> Result<Self, Self::Error> {
        let (op, span) = op.into();
        use ast::UnaryOp::*;
        let op = match op {
            NOT => Self::NOT,
            INV => Self::INV,
            NEG => Self::NEG { is_float: false },
            FNEG => Self::NEG { is_float: true },
            _ => return Err(LiftError::Invalid { span }),
        };
        Ok(op)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Id {
    pub ident: Ident,
    pub tag: Tag,
}

impl Id {
    pub fn new(ident: Ident, span: Span) -> Self {
        Self {
            ident,
            tag: Tag::from(span),
        }
    }
}

impl From<Loc<Ident>> for Id {
    fn from(value: Loc<Ident>) -> Self {
        let (ident, span) = value.into();
        Self::new(ident, span)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct MemoryRegion {
    pub id: Id,
    pub kind: ast::SpaceKind,
    pub size: usize,
    pub word_size: usize,
    pub is_default: bool,
}

impl TryFrom<&Type> for Ptr<MemoryRegion> {
    type Error = LiftError;

    fn try_from(ty: &Type) -> Result<Self, Self::Error> {
        if let Type::MemoryRegion(x) = ty {
            Ok(x.clone())
        } else {
            Err(LiftError::InternalTypeMismatch)
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Register {
    pub id: Id,
    pub size: usize,
}

impl TryFrom<&Type> for Ptr<Register> {
    type Error = LiftError;

    fn try_from(ty: &Type) -> Result<Self, Self::Error> {
        if let Type::Register(x) = ty {
            Ok(x.clone())
        } else {
            Err(LiftError::InternalTypeMismatch)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RegisterMap {
    pub registers: Vec<Option<Ptr<Register>>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct BitField {
    pub id: Id,
    pub bit_width: usize,
    pub start_bit: usize,
    pub end_bit: usize,
    pub is_signed: bool,
    pub is_hex: bool,
}

impl From<ast::Field> for BitField {
    fn from(field: ast::Field) -> Self {
        Self {
            id: Id::from(field.id),
            bit_width: 0,
            start_bit: field.start_bit.into_value(),
            end_bit: field.end_bit.into_value(),
            is_signed: field.is_signed,
            is_hex: field.is_hex,
        }
    }
}

impl TryFrom<&Type> for Ptr<BitField> {
    type Error = LiftError;

    fn try_from(ty: &Type) -> Result<Self, Self::Error> {
        if let Type::BitField(x) = ty {
            Ok(x.clone())
        } else {
            Err(LiftError::InternalTypeMismatch)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RegisterIndex {
    pub bit_field: Ptr<BitField>,
    pub register_map_idx: usize,
}

impl TryFrom<&Type> for Ptr<RegisterIndex> {
    type Error = LiftError;

    fn try_from(ty: &Type) -> Result<Self, Self::Error> {
        if let Type::RegisterIndex(x) = ty {
            Ok(x.clone())
        } else {
            Err(LiftError::InternalTypeMismatch)
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Variable {
    pub id: Id,
}

impl TryFrom<&Type> for Ptr<Variable> {
    type Error = LiftError;

    fn try_from(ty: &Type) -> Result<Self, Self::Error> {
        if let Type::Variable(x) = ty {
            Ok(x.clone())
        } else {
            Err(LiftError::InternalTypeMismatch)
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Intrinsic {
    pub id: Id,
}

impl TryFrom<&Type> for Ptr<Intrinsic> {
    type Error = LiftError;

    fn try_from(ty: &Type) -> Result<Self, Self::Error> {
        if let Type::Intrinsic(x) = ty {
            Ok(x.clone())
        } else {
            Err(LiftError::InternalTypeMismatch)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum JumpTarget {
    Fixed(Address),
    Direct(Loc<Ident>),
    Indirect(Expr),
    Label(Loc<Ident>),
}

impl JumpTarget {
    pub fn lift(scope: &Scope, target: ast::JumpTarget) -> Result<Self, LiftError> {
        use ast::JumpTarget::*;
        let target = match target {
            Fixed { address, space } => {
                let region = match space {
                    Some(id) => {
                        let region = scope.lookup(&id)?.try_into()?;
                        Some(region)
                    }
                    None => None,
                };
                let addr = Address {
                    address: address.into_value(),
                    region,
                };
                JumpTarget::Fixed(addr)
            }
            Direct(id) => JumpTarget::Direct(id), // TODO: Validate in this env!
            Indirect(expr) => JumpTarget::Indirect(Expr::lift(scope, expr)?),
            Label(id) => JumpTarget::Label(id), // TODO: Validate in this env!
        };
        Ok(target)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Address {
    pub address: usize,
    pub region: Option<Ptr<MemoryRegion>>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Statement {
    Bind {
        lhs: Expr,
        rhs: Expr,
    },
    MacroCall {
        m: Ptr<Macro>,
        args: Vec<Expr>,
    },
    FunCall {
        intrinsic: Ptr<Intrinsic>,
        args: Vec<Expr>,
    },
    Transfer(Transfer),
    Branch {
        condition: Expr,
        target: JumpTarget,
    },
    Export(Expr),
    Build(PtrMut<Scanner>),
    Label(Id),
}

impl Statement {
    pub fn lift(scope: &mut Scope, stmt: ast::Statement) -> Result<Statement, LiftError> {
        use ast::Statement::*;
        match stmt {
            Bind { lhs, rhs } => {
                scope.add_vars(&lhs, None)?;
                let stmt = Statement::Bind {
                    lhs: Expr::lift(scope, lhs)?,
                    rhs: Expr::lift(scope, rhs)?,
                };
                Ok(stmt)
            }
            Goto(target) => {
                let target = JumpTarget::lift(scope, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Goto,
                    target,
                };
                let stmt = Statement::Transfer(xfer);
                Ok(stmt)
            }
            Call(target) => {
                let target = JumpTarget::lift(scope, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Call,
                    target,
                };
                let stmt = Statement::Transfer(xfer);
                Ok(stmt)
            }
            Return(target) => {
                let target = JumpTarget::lift(scope, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Return,
                    target,
                };
                let stmt = Statement::Transfer(xfer);
                Ok(stmt)
            }
            Branch { condition, target } => {
                let condition = Expr::lift(scope, condition)?;
                let target = JumpTarget::lift(scope, target)?;
                let stmt = Statement::Branch { condition, target };
                Ok(stmt)
            }
            FunCall { id, args } => {
                let intrinsic = scope.lookup(&id)?.try_into()?;
                let args: Result<Vec<Expr>, _> =
                    args.into_iter().map(|arg| Expr::lift(scope, arg)).collect();
                let stmt = Statement::FunCall {
                    intrinsic,
                    args: args?,
                };
                Ok(stmt)
            }
            Export(expr) => {
                let expr = Expr::lift(scope, expr)?;
                let stmt = Statement::Export(expr);
                Ok(stmt)
            }
            Label(id) => {
                let stmt = Statement::Label(Id::from(id));
                Ok(stmt)
            }
            Build(id) => {
                let scanner: PtrMut<Scanner> = scope.lookup(&id)?.try_into()?;
                let stmt = Statement::Build(scanner.clone());
                Ok(stmt)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Transfer {
    pub kind: TransferKind,
    pub target: JumpTarget,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TransferKind {
    Call,
    Goto,
    Return,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Macro {
    pub id: Id,
    pub args: Vec<Id>,
    pub body: Vec<Statement>,
    pub scope: Scope,
}

impl TryFrom<&Type> for Ptr<Macro> {
    type Error = LiftError;

    fn try_from(ty: &Type) -> Result<Self, Self::Error> {
        if let Type::Macro(x) = ty {
            Ok(x.clone())
        } else {
            Err(LiftError::InternalTypeMismatch)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct PCodeOp {
    pub id: Id,
}

impl TryFrom<&Type> for Ptr<PCodeOp> {
    type Error = LiftError;

    fn try_from(ty: &Type) -> Result<Self, Self::Error> {
        if let Type::PCodeOp(x) = ty {
            Ok(x.clone())
        } else {
            Err(LiftError::InternalTypeMismatch)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Scanner {
    pub id: Id,
    pub rules: Vec<PtrMut<Rule>>,
    pub is_instruction: bool,
}

impl TryFrom<&Type> for PtrMut<Scanner> {
    type Error = LiftError;

    fn try_from(ty: &Type) -> Result<Self, Self::Error> {
        if let Type::Scanner(x) = ty {
            Ok(x.clone())
        } else {
            Err(LiftError::InternalTypeMismatch)
        }
    }
}

type Pattern = Vec<PtrMut<Expr>>;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Rule {
    pub id: Id,
    pub mnemonic: Vec<PtrMut<Output>>,
    pub output: Vec<PtrMut<Output>>,
    pub setup: Vec<Statement>,
    pub actions: Vec<Statement>,
    pub pattern: Pattern,
    pub scope: Scope,
}

impl Rule {
    pub fn lift(mut scope: Scope, ctr: ast::Constructor) -> Result<Self, LiftError> {
        let setup: Result<Vec<Statement>, LiftError> = ctr
            .context
            .into_iter()
            .map(|stmt| Statement::lift(&mut scope, stmt))
            .collect();
        let pattern = Self::lift_pattern(&scope, ctr.pattern)?;
        let mnemonic: Vec<PtrMut<Output>> = ctr
            .display
            .mnemonic
            .into_iter()
            .map(|piece| Output::lift(&scope, piece))
            .collect::<Result<Vec<Option<Output>>, LiftError>>()?
            .into_iter()
            .flatten()
            .map(|x| Rc::new(RefCell::new(x)))
            .collect();
        let output: Vec<PtrMut<Output>> = ctr
            .display
            .output
            .into_iter()
            .map(|piece| Output::lift(&scope, piece))
            .collect::<Result<Vec<Option<Output>>, LiftError>>()?
            .into_iter()
            .flatten()
            .map(|x| Rc::new(RefCell::new(x)))
            .collect();
        let actions: Result<Vec<Statement>, LiftError> = ctr
            .body
            .into_iter()
            .map(|stmt| Statement::lift(&mut scope, stmt))
            .collect();
        let rule = Self {
            id: Id::from(ctr.id),
            mnemonic,
            output,
            setup: setup?,
            actions: actions?,
            pattern,
            scope,
        };
        Ok(rule)
    }

    fn lift_expr(scope: &Scope, acc: &mut Pattern, expr: ast::Expr) -> Result<(), LiftError> {
        use ast::Expr::*;
        match expr {
            Binary { op, lhs, rhs } if op.value() == &ast::BinaryOp::JOIN => {
                let lhs = Expr::lift(scope, *lhs)?;
                let rhs = Expr::lift(scope, *rhs)?;
                acc.push(Rc::new(RefCell::new(lhs)));
                acc.push(Rc::new(RefCell::new(rhs)));
            }
            expr => return Err(LiftError::Invalid { span: *expr.span() }),
        }
        Ok(())
    }

    fn lift_pattern(scope: &Scope, expr: Option<ast::Expr>) -> Result<Pattern, LiftError> {
        let mut pattern = Vec::new();
        if let Some(expr) = expr {
            Self::lift_expr(scope, &mut pattern, expr)?;
        }
        Ok(pattern)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Output {
    Text(Ident),
    Register(Ptr<Register>),
    RegisterIndex(Ptr<RegisterIndex>),
    BitField(Ptr<BitField>),
    Scanner(PtrMut<Scanner>),
}

impl Output {
    pub fn lift(scope: &Scope, piece: ast::DisplayPiece) -> Result<Option<Self>, LiftError> {
        use ast::DisplayPiece::*;
        let out = match piece {
            Text(id) => Some(Output::Text(id)),
            Caret | Space => None,
            Id(id) => match scope.lookup(&id)? {
                Type::Register(x) => Some(Output::Register(x.clone())),
                Type::RegisterIndex(x) => Some(Output::RegisterIndex(x.clone())),
                Type::BitField(x) => Some(Output::BitField(x.clone())),
                Type::Scanner(x) => Some(Output::Scanner(x.clone())),
                _ => return Err(LiftError::Invalid { span: id.span() }),
            },
        };
        Ok(out)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Scope {
    pub parent_env: Option<TypeEnv>,
    pub env: TypeEnv,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            parent_env: None,
            env: TypeEnv::new(),
        }
    }
}

impl Scope {
    pub fn to_local(&self) -> Self {
        Self {
            parent_env: Some(self.env.clone()),
            env: TypeEnv::new(),
        }
    }

    pub fn lookup(&self, id: &Loc<Ident>) -> Result<&Type, LiftError> {
        let span = id.tag();
        let id = id.value();
        self.parent_env
            .as_ref()
            .and_then(|env| env.get(id))
            .or_else(|| self.env.get(id))
            .ok_or(LiftError::Unknown { span: *span })
    }

    fn to_expr(&self, id: &Loc<Ident>) -> Result<Expr, LiftError> {
        let ty = self.lookup(id)?;
        let span = *id.tag();
        use Expr::*;
        match ty {
            Type::Register(x) => Ok(Register(x.clone())),
            Type::RegisterIndex(x) => Ok(RegisterIndex(x.clone())),
            Type::BitField(x) => Ok(BitField(x.clone())),
            Type::Scanner(x) => Ok(Scanner(x.clone())),
            Type::Variable(x) => Ok(Variable(x.clone())),
            _ => Err(LiftError::Invalid { span }),
        }
    }

    pub fn insert(&mut self, id: Ident, value: Type) {
        self.env.insert(id, value)
    }

    pub fn len(&self) -> usize {
        self.env.len()
    }

    pub fn is_empty(&self) -> bool {
        self.env.is_empty()
    }

    pub fn add_vars(
        &mut self,
        expr: &ast::Expr,
        expected_size: Option<usize>,
    ) -> Result<(), LiftError> {
        use ast::Expr::*;
        match expr {
            Sized { expr, size } => {
                let size = *size.value();
                let span = *expr.span();
                if let Some(expected) = expected_size {
                    if size != expected {
                        return Err(LiftError::TypeMismatch { span });
                    }
                }
                self.add_vars(expr, Some(size))?
            }
            Pointer { expr, .. } => self.add_vars(expr, expected_size)?,
            Id(id) => {
                let id_ = id.value();
                let id = self::Id::from(*id);
                if self
                    .parent_env
                    .as_ref()
                    .and_then(|env| env.get(id_))
                    .or_else(|| self.env.get(id_))
                    .is_some()
                {
                    self.insert(*id_, Type::Variable(Rc::new(Variable { id })))
                }
            }
            _ => {}
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Architecture {
    pub endian: Endian,
    pub alignment: usize,
    pub scope: Scope,
    pub default_region: Option<Ptr<MemoryRegion>>,
    pub scanners: Vec<PtrMut<Scanner>>,
    pub register_maps: Vec<RegisterMap>,
}

impl Architecture {
    pub fn new() -> Self {
        Self {
            endian: Endian::Little,
            alignment: 4,
            scope: Scope::default(),
            default_region: None,
            scanners: Vec::new(),
            register_maps: Vec::new(),
        }
    }

    pub fn lift(&mut self, defs: Vec<ast::Definition>) -> Result<(), LiftError> {
        use ast::Definition as Def;
        for def in defs {
            match def {
                Def::Endian(x) => self.lift_endian(x)?,
                Def::Alignment(x) => self.lift_alignment(x)?,
                Def::Space(x) => self.lift_memory_region(x)?,
                Def::Token(x) => self.lift_bit_fields(x)?,
                Def::Varnode(x) => self.lift_registers(x)?,
                Def::VarnodeAttach(x) => self.lift_register_map(x)?,
                Def::PCodeOp(x) => self.lift_pcode_op(x)?,
                Def::Constructor(x) => self.lift_scanner(x)?,
                Def::Macro(x) => self.lift_macro(x)?,
            }
        }
        Ok(())
    }

    fn lift_endian(&mut self, endian: Loc<ast::Endian>) -> Result<(), LiftError> {
        self.endian = endian.into_value();
        Ok(())
    }

    fn lift_alignment(&mut self, alignment: Loc<usize>) -> Result<(), LiftError> {
        self.alignment = alignment.into_value();
        Ok(())
    }

    fn lift_memory_region(&mut self, space: ast::Space) -> Result<(), LiftError> {
        let id = Id::from(space.id);
        let region = MemoryRegion {
            id,
            kind: space.kind,
            size: space.size.into_value(),
            word_size: space.word_size.into_value(),
            is_default: space.is_default,
        };
        self.scope
            .insert(id.ident, Type::MemoryRegion(Rc::new(region)));
        Ok(())
    }

    fn lift_bit_fields(&mut self, token: ast::Token) -> Result<(), LiftError> {
        let bit_width = token.bit_width;
        for field in token.fields {
            let mut field = BitField::from(field);
            field.bit_width = bit_width.into_value();
            self.scope
                .insert(field.id.ident, Type::BitField(Rc::new(field)));
        }
        Ok(())
    }

    fn lift_registers(&mut self, varnode: Loc<ast::Varnode>) -> Result<(), LiftError> {
        let varnode = varnode.into_value();
        for id in varnode.ids {
            let register = Register {
                id: Id::from(id),
                size: varnode.byte_size.into_value(),
            };
            self.scope
                .insert(id.into_value(), Type::Register(Rc::new(register)));
        }
        Ok(())
    }

    fn lift_register_map(&mut self, attach: Loc<ast::VarnodeAttach>) -> Result<(), LiftError> {
        let underscore = Ident::new("_");
        let attach = attach.into_value();
        // Create register map
        let mut registers = Vec::new();
        for id in attach.registers {
            let maybe_register = if id.value() != &underscore {
                let register = self.scope.lookup(&id)?.try_into()?;
                Some(register)
            } else {
                None
            };
            registers.push(maybe_register);
        }
        let register_map_idx = self.register_maps.len();
        self.register_maps.push(RegisterMap { registers });
        // Create register indices that refer to the map
        for id in attach.fields {
            let bit_field = self.scope.lookup(&id)?.try_into()?;
            let index = RegisterIndex {
                register_map_idx,
                bit_field,
            };
            self.scope
                .insert(id.into_value(), Type::RegisterIndex(Rc::new(index)));
        }
        Ok(())
    }

    fn lift_pcode_op(&mut self, id: Loc<Ident>) -> Result<(), LiftError> {
        let id = Id::from(id);
        let op = PCodeOp { id };
        self.scope.insert(id.ident, Type::PCodeOp(Rc::new(op)));
        Ok(())
    }

    fn lift_macro(&mut self, m: ast::Macro) -> Result<(), LiftError> {
        let (id, span) = m.id.into();
        let mut scope = self.scope.to_local();
        let args = m.args.into_iter().map(Id::from).collect();
        let body: Result<Vec<Statement>, _> = m
            .body
            .into_iter()
            .map(|stmt| Statement::lift(&mut scope, stmt))
            .collect();
        let m = Macro {
            id: Id::new(id, span),
            args,
            body: body?,
            scope,
        };
        self.scope.insert(id, Type::Macro(Rc::new(m)));
        Ok(())
    }

    fn lift_scanner(&mut self, ctr: ast::Constructor) -> Result<(), LiftError> {
        let (id, span) = ctr.id.into();
        let is_instruction = ctr.is_instruction;
        let result = self
            .scope
            .lookup(&ctr.id)
            .and_then(|x| x.try_into())
            .map(|x: Rc<RefCell<Scanner>>| Some(x))
            .or(Ok(None));
        let result = match (is_instruction, result?) {
            (true, Some(_)) => Err(LiftError::Duplicate { span }),
            (_, None) => {
                let scanner = Scanner {
                    id: self::Id::from(ctr.id),
                    is_instruction,
                    rules: Vec::new(),
                };
                let scanner = Rc::new(RefCell::new(scanner));
                let clone = scanner.clone();
                self.scope.insert(id, Type::Scanner(scanner));
                Ok(clone)
            }
            (false, Some(scanner)) => Ok(scanner.clone()),
        };
        let scanner = result?;
        let mut scanner = scanner.borrow_mut();
        let rule = Rule::lift(self.scope.to_local(), ctr)?;
        scanner.rules.push(Rc::new(RefCell::new(rule)));
        Ok(())
    }
}

impl Default for Architecture {
    fn default() -> Self {
        Self::new()
    }
}
