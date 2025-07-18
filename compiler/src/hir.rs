use crate::error::LiftError;
use bebop_parser::ast;
use bebop_util::meta::*;
use bitflags::bitflags;
use core::ops;
use ordermap::{OrderMap, OrderSet};
use serde::{Serialize, Serializer};
use std::hash::Hash;
use std::option::Option::*;

pub type Ident = ast::Ident;
pub type Endian = ast::Endian;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Id(Tagged<Ident, Meta>);

impl Id {
    pub fn new(id: Ident) -> Self {
        Self(Tagged::new(id, Meta::default()))
    }

    pub fn ident(&self) -> &Ident {
        self.0.value()
    }
}

impl From<&Loc<Ident>> for Id {
    fn from(id: &Loc<Ident>) -> Self {
        let inner = id.map_tag(Meta::new);
        Self(inner)
    }
}

impl From<Loc<Ident>> for Id {
    fn from(id: Loc<Ident>) -> Self {
        Self::from(&id)
    }
}

impl Spanned for Id {
    fn span(&self) -> Span {
        self.0.tag().span()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Serialize)]
pub struct Meta {
    pub size: Option<usize>,
    pub hint: Option<Hint>,
    #[serde(skip_serializing)]
    pub span: Span,
}

impl Meta {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            ..Default::default()
        }
    }
}

impl Spanned for Meta {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Hint {
    Unsigned,
    Signed,
    Float,
}

bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Hash)]
    pub struct Types: u8 {
        const MemoryRegion  = 0b00000001;
        const Register      = 0b00000010;
        const RegisterIndex = 0b00000100;
        const BitField      = 0b00001000;
        const Intrinsic     = 0b00010000;
        const Macro         = 0b00100000;
        const Scanner       = 0b01000000;
        const Variable      = 0b10000000;
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExprId(id_arena::Id<Expr>);

type LiftResult = Result<ExprId, LiftError>;

#[derive(Debug)]
pub struct ExprPool {
    arena: id_arena::Arena<Expr>,
}

impl Default for ExprPool {
    fn default() -> Self {
        Self {
            arena: id_arena::Arena::<Expr>::new(),
        }
    }
}

impl ExprPool {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn add(&mut self, expr: Expr) -> ExprId {
        ExprId(self.arena.alloc(expr))
    }

    fn span(&self, id: &ExprId) -> Span {
        use Expr::*;
        match &self[id] {
            Binary { lhs, .. } => self.span(lhs),
            Unary { rhs, .. } => self.span(rhs),
            Paren(expr) => self.span(expr),
            Pointer { expr, .. } => self.span(expr),
            TakeBits { id, .. } => id.span(),
            TakeBytes { expr, .. } => self.span(expr),
            FunCall { intrinsic, .. } => self.span(intrinsic),
            Register(reg) => reg.id.span(),
            RegisterIndex(index) => self.span(&index.bit_field),
            BitField(field) => field.id.span(),
            Scanner(scanner) => scanner.id.span(),
            Variable(var) => var.id.span(),
            Int(_) | Unit => Span::default(),
            Bind { lhs, .. } => self.span(lhs),
            MacroCall { m, .. } => self.span(m),
            Transfer(xfer) => xfer.span(self),
            Branch { condition, .. } => self.span(condition),
            Export(expr) => self.span(expr),
            Build(expr) => self.span(expr),
            Label(id) => id.span(),
            MemoryRegion(region) => region.id.span(),
            Intrinsic(intrinsic) => intrinsic.id.span(),
            Macro(m) => m.id.span(),
            Rule(rule) => rule.id.span(),
        }
    }
}

impl ops::Index<&ExprId> for ExprPool {
    type Output = Expr;

    #[inline]
    fn index(&self, id: &ExprId) -> &Expr {
        &self.arena[id.0]
    }
}

impl ops::IndexMut<&ExprId> for ExprPool {
    #[inline]
    fn index_mut(&mut self, id: &ExprId) -> &mut Expr {
        &mut self.arena[id.0]
    }
}

impl Serialize for ExprPool {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let a: Vec<(usize, &Expr)> = self
            .arena
            .iter()
            .map(|(id, expr)| (id.index(), expr))
            .collect();
        a.serialize(serializer)
    }
}

impl Serialize for ExprId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.index().serialize(serializer)
    }
}

#[derive(Debug, PartialEq, Serialize)]
pub struct TypeEnv {
    env: OrderMap<Ident, OrderSet<(Types, ExprId)>>,
}

impl Clone for TypeEnv {
    fn clone(&self) -> Self {
        Self {
            env: self.env.clone(),
        }
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self {
            env: OrderMap::new(),
        }
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, id: Ident, expr_id: ExprId, types: Types) {
        self.env
            .entry(id)
            .and_modify(|values| _ = values.insert((types, expr_id)))
            .or_default();
    }

    pub fn get(&self, id: &Ident, types: Types) -> Option<ExprId> {
        self.env.get(id).and_then(|values| {
            values
                .iter()
                .find(|(value_types, _)| value_types.intersects(types))
                .map(|(_, id)| *id)
        })
    }

    pub fn len(&self) -> usize {
        self.env.len()
    }

    pub fn is_empty(&self) -> bool {
        self.env.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expr {
    Binary {
        op: BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    },
    Unary {
        op: UnaryOp,
        rhs: ExprId,
    },
    Paren(ExprId),
    Pointer {
        expr: ExprId,
        region: Option<ExprId>,
    },
    TakeBits {
        id: Id,
        start_bit: usize,
        bit_width: usize,
    },
    TakeBytes {
        expr: ExprId,
        n: usize,
    },
    FunCall {
        intrinsic: ExprId,
        args: Vec<ExprId>,
    },
    Register(Register),
    RegisterIndex(RegisterIndex),
    BitField(BitField),
    Scanner(Scanner),
    Variable(Variable),
    Int(usize),
    Unit,
    // Statements
    Bind {
        lhs: ExprId,
        rhs: ExprId,
    },
    MacroCall {
        m: ExprId,
        args: Vec<ExprId>,
    },
    Transfer(Transfer),
    Branch {
        condition: ExprId,
        target: JumpTarget,
    },
    Export(ExprId),
    Build(ExprId),
    Label(Id),
    // Invalid expressions, for pooling only!
    MemoryRegion(MemoryRegion),
    Intrinsic(Intrinsic),
    Macro(Macro),
    Rule(Rule),
}

impl Expr {
    pub fn lift(
        pool: &mut ExprPool,
        scope: &Scope,
        expr: &ast::Expr,
    ) -> LiftResult {
        use ast::Expr::*;
        match expr {
            Binary { op, lhs, rhs } => {
                let lhs = Self::lift(pool, scope, lhs)?;
                let rhs = Self::lift(pool, scope, rhs)?;
                let op = BinaryOp::try_from(op)?;
                let expr = Expr::Binary { op, lhs, rhs };
                Ok(pool.add(expr))
            }
            Unary { op, rhs } => {
                let rhs = Self::lift(pool, scope, rhs)?;
                let op = UnaryOp::try_from(op)?;
                let expr = Expr::Unary { op, rhs };
                Ok(pool.add(expr))
            }
            Paren(expr) => {
                let id = Self::lift(pool, scope, expr)?;
                let expr = Expr::Paren(id);
                Ok(pool.add(expr))
            }
            FunCall { id, args } => {
                let intrinsic = scope.lookup(id, Types::Intrinsic)?;
                let args: Result<Vec<ExprId>, _> = args
                    .iter()
                    .map(|arg| Self::lift(pool, scope, arg))
                    .collect();
                let expr = Expr::FunCall {
                    intrinsic,
                    args: args?,
                };
                Ok(pool.add(expr))
            }
            BitRange {
                id,
                start_bit,
                bit_width,
            } => {
                let expr = Expr::TakeBits {
                    id: id.into(),
                    start_bit: start_bit.into_value(),
                    bit_width: bit_width.into_value(),
                };
                Ok(pool.add(expr))
            }
            Sized { expr, size } => {
                let id = Self::lift(pool, scope, expr)?;
                let expr = Expr::TakeBytes {
                    expr: id,
                    n: size.into_value(),
                };
                Ok(pool.add(expr))
            }
            Pointer { expr, space } => {
                let expr = Self::lift(pool, scope, expr)?;
                let region = space
                    .map(|id| scope.lookup(&id, Types::MemoryRegion))
                    .transpose()?;
                let expr = Expr::Pointer { expr, region };
                Ok(pool.add(expr))
            }
            Id(id) => scope.lookup(id, Types::all()),
            Int(n) => {
                let expr = Expr::Int(n.into_value());
                Ok(pool.add(expr))
            }
            Unit(_) => Ok(pool.add(Expr::Unit)),
        }
    }

    pub fn lift_stmt(
        pool: &mut ExprPool,
        scope: &mut Scope,
        stmt: ast::Statement,
    ) -> LiftResult {
        use ast::Statement::*;
        match stmt {
            Bind { lhs, rhs } => {
                scope.add_local_vars(pool, &lhs, None)?;
                let lhs = Self::lift(pool, scope, &lhs)?;
                let rhs = Self::lift(pool, scope, &rhs)?;
                let expr = Expr::Bind { lhs, rhs };
                Ok(pool.add(expr))
            }
            FunCall { id, args } => {
                let intrinsic = scope.lookup(&id, Types::Intrinsic)?;
                let args: Result<Vec<ExprId>, _> = args
                    .into_iter()
                    .map(|arg| Self::lift(pool, scope, &arg))
                    .collect();
                let expr = Expr::FunCall {
                    intrinsic,
                    args: args?,
                };
                Ok(pool.add(expr))
            }
            Goto(target) => {
                let target = JumpTarget::lift(pool, scope, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Goto,
                    target,
                };
                let expr = Expr::Transfer(xfer);
                Ok(pool.add(expr))
            }
            Call(target) => {
                let target = JumpTarget::lift(pool, scope, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Call,
                    target,
                };
                let expr = Expr::Transfer(xfer);
                Ok(pool.add(expr))
            }
            Return(target) => {
                let target = JumpTarget::lift(pool, scope, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Return,
                    target,
                };
                let expr = Expr::Transfer(xfer);
                Ok(pool.add(expr))
            }
            Branch { condition, target } => {
                let condition = Expr::lift(pool, scope, &condition)?;
                let target = JumpTarget::lift(pool, scope, target)?;
                let expr = Expr::Branch { condition, target };
                Ok(pool.add(expr))
            }
            Export(expr) => {
                let expr = Expr::lift(pool, scope, &expr)?;
                let expr = Expr::Export(expr);
                Ok(pool.add(expr))
            }
            Label(id) => {
                let expr = Expr::Label(id.into());
                Ok(pool.add(expr))
            }
            Build(id) => {
                let scanner = scope.lookup(&id, Types::Scanner)?;
                let expr = Expr::Build(scanner);
                Ok(pool.add(expr))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
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

impl TryFrom<&Loc<ast::BinaryOp>> for BinaryOp {
    type Error = LiftError;

    fn try_from(op: &Loc<ast::BinaryOp>) -> Result<Self, Self::Error> {
        let (op, span) = (op.value(), op.tag());
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
            _ => return Err(LiftError::Invalid(*span)),
        };
        Ok(op)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum UnaryOp {
    NOT,
    INV,
    NEG { is_float: bool },
}

impl TryFrom<&Loc<ast::UnaryOp>> for UnaryOp {
    type Error = LiftError;

    fn try_from(op: &Loc<ast::UnaryOp>) -> Result<Self, Self::Error> {
        let (op, span) = (op.value(), op.tag());
        use ast::UnaryOp::*;
        let op = match op {
            NOT => Self::NOT,
            INV => Self::INV,
            NEG => Self::NEG { is_float: false },
            FNEG => Self::NEG { is_float: true },
            _ => return Err(LiftError::Invalid(*span)),
        };
        Ok(op)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MemoryRegion {
    pub id: Id,
    pub kind: ast::SpaceKind,
    pub size: usize,
    pub word_size: usize,
    pub is_default: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Register {
    pub id: Id,
    pub size: usize,
}

#[derive(Debug, Clone, Serialize)]
pub struct RegisterMap {
    pub registers: Vec<Option<ExprId>>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
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
            id: field.id.into(),
            bit_width: 0,
            start_bit: field.start_bit.into_value(),
            end_bit: field.end_bit.into_value(),
            is_signed: field.is_signed,
            is_hex: field.is_hex,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RegisterIndex {
    pub bit_field: ExprId,
    pub register_map_idx: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Variable {
    pub id: Id,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Intrinsic {
    pub id: Id,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum JumpTarget {
    Fixed(Address),
    Direct(ExprId),
    Indirect(ExprId),
    Label(Loc<Ident>),
}

impl JumpTarget {
    pub fn lift(
        pool: &mut ExprPool,
        scope: &Scope,
        target: ast::JumpTarget,
    ) -> Result<Self, LiftError> {
        use ast::JumpTarget::*;
        let target = match target {
            Fixed { address, space } => {
                let region = space
                    .map(|id| scope.lookup(&id, Types::MemoryRegion))
                    .transpose()?;
                let addr = Address {
                    address: address.into_value(),
                    region,
                };
                JumpTarget::Fixed(addr)
            }
            Direct(id) => {
                let expr = scope.lookup(&id, Types::all())?;
                JumpTarget::Direct(expr)
            }
            Indirect(expr) => {
                let expr = Expr::lift(pool, scope, &expr)?;
                JumpTarget::Indirect(expr)
            }
            Label(id) => JumpTarget::Label(id), // TODO: Validate in this env!
        };
        Ok(target)
    }

    pub fn span(&self, pool: &ExprPool) -> Span {
        use JumpTarget::*;
        match self {
            Fixed(_) => Span::default(),
            Direct(expr) => pool.span(expr),
            Indirect(expr) => pool.span(expr),
            Label(id) => id.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Address {
    pub address: usize,
    pub region: Option<ExprId>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Transfer {
    pub kind: TransferKind,
    pub target: JumpTarget,
}

impl Transfer {
    pub fn span(&self, pool: &ExprPool) -> Span {
        self.target.span(pool)
    }
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
    pub body: Vec<ExprId>,
    pub scope: Scope,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Scanner {
    pub id: Id,
    pub rules: Vec<ExprId>,
    pub is_instruction: bool,
}

type Pattern = Vec<ExprId>;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Rule {
    pub id: Id,
    pub mnemonic: Vec<Output>,
    pub output: Vec<Output>,
    pub setup: Vec<ExprId>,
    pub actions: Vec<ExprId>,
    pub pattern: Pattern,
    pub scope: Scope,
}

impl Rule {
    pub fn lift(
        pool: &mut ExprPool,
        mut scope: Scope,
        ctr: ast::Constructor,
    ) -> Result<Self, LiftError> {
        let setup: Result<Vec<ExprId>, LiftError> = ctr
            .context
            .into_iter()
            .map(|stmt| Expr::lift_stmt(pool, &mut scope, stmt))
            .collect();
        let pattern = Self::lift_pattern(pool, &scope, ctr.pattern)?;
        let mnemonic: Vec<Output> = ctr
            .display
            .mnemonic
            .into_iter()
            .map(|piece| Output::lift(&scope, piece))
            .collect::<Result<Vec<Option<Output>>, LiftError>>()?
            .into_iter()
            .flatten()
            .collect();
        let output: Vec<Output> = ctr
            .display
            .output
            .into_iter()
            .map(|piece| Output::lift(&scope, piece))
            .collect::<Result<Vec<Option<Output>>, LiftError>>()?
            .into_iter()
            .flatten()
            .collect();
        let actions: Result<Vec<ExprId>, LiftError> = ctr
            .body
            .into_iter()
            .map(|stmt| Expr::lift_stmt(pool, &mut scope, stmt))
            .collect();
        let rule = Self {
            id: ctr.id.into(),
            mnemonic,
            output,
            setup: setup?,
            actions: actions?,
            pattern,
            scope,
        };
        Ok(rule)
    }

    fn lift_pat_expr(
        pool: &mut ExprPool,
        scope: &Scope,
        acc: &mut Pattern,
        expr: ast::Expr,
    ) -> Result<(), LiftError> {
        use ast::Expr::*;
        match expr {
            Binary { op, lhs, rhs }
                if op.value() == &ast::BinaryOp::JOIN =>
            {
                let lhs = Expr::lift(pool, scope, &lhs)?;
                let rhs = Expr::lift(pool, scope, &rhs)?;
                acc.push(lhs);
                acc.push(rhs);
            }
            expr => {
                let expr = Expr::lift(pool, scope, &expr)?;
                acc.push(expr);
            }
        }
        Ok(())
    }

    fn lift_pattern(
        pool: &mut ExprPool,
        scope: &Scope,
        expr: Option<ast::Expr>,
    ) -> Result<Pattern, LiftError> {
        let mut pattern = Vec::new();
        expr.map(|expr| {
            Self::lift_pat_expr(pool, scope, &mut pattern, expr)
        })
        .transpose()?;
        Ok(pattern)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Output {
    Text(Ident),
    Expr(ExprId),
}

impl Output {
    pub fn lift(
        scope: &Scope,
        piece: ast::DisplayPiece,
    ) -> Result<Option<Self>, LiftError> {
        use ast::DisplayPiece::*;
        let out = match piece {
            Text(id) => Some(Output::Text(id)),
            Caret | Space => None,
            Id(id) => {
                let expr = scope.lookup(&id, Types::all())?;
                Some(Output::Expr(expr))
            }
        };
        Ok(out)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Scope {
    // Enabling serialization of `parent_env` creates a circular reference
    // and overflows the stack. This is because `Rule.parent_env` points
    // to the global scope which embdeds the rule. We don't need to dump
    // the parent env of a rule anyway since it will be dumped as part
    // of the Architecture.
    #[serde(skip_serializing)]
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

    pub fn lookup(&self, id: &Loc<Ident>, types: Types) -> LiftResult {
        let span = id.span();
        let id = id.value();
        self.parent_env
            .as_ref()
            .and_then(|env| env.get(id, types))
            .or_else(|| self.env.get(id, types))
            .ok_or(LiftError::Unknown(span))
    }

    pub fn insert(&mut self, id: Ident, expr_id: ExprId, types: Types) {
        self.env.insert(id, expr_id, types)
    }

    pub fn len(&self) -> usize {
        self.env.len()
    }

    pub fn is_empty(&self) -> bool {
        self.env.is_empty()
    }

    pub fn add_local_vars(
        &mut self,
        pool: &mut ExprPool,
        expr: &ast::Expr,
        expected_size: Option<usize>,
    ) -> Result<(), LiftError> {
        use ast::Expr::*;
        match expr {
            Sized { expr, size } => {
                let size = *size.value();
                let span = expr.span();
                if expected_size.is_some_and(|expected| size != expected) {
                    return Err(LiftError::SizeMismatch {
                        span,
                        want: expected_size.unwrap(),
                        got: size,
                    });
                }
                self.add_local_vars(pool, expr, Some(size))?
            }
            Pointer { expr, .. } => {
                self.add_local_vars(pool, expr, expected_size)?
            }
            Id(id) => {
                let id_ = id.value();
                let id = id.into();
                if self
                    .parent_env
                    .as_ref()
                    .and_then(|env| env.get(id_, Types::Variable))
                    .or_else(|| self.env.get(id_, Types::Variable))
                    .is_none()
                {
                    let expr = pool.add(Expr::Variable(Variable { id }));
                    self.insert(*id_, expr, Types::Variable);
                }
            }
            _ => {}
        }
        Ok(())
    }
}

#[derive(Debug, Serialize)]
pub struct Architecture {
    pub endian: Endian,
    pub alignment: usize,
    pub scope: Scope,
    pub default_region: Option<ExprId>,
    pub register_maps: Vec<RegisterMap>,
    pub pool: ExprPool,
}

impl Architecture {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn lift(
        &mut self,
        defs: Vec<ast::Definition>,
    ) -> Result<(), LiftError> {
        use ast::Definition as Def;
        for def in defs {
            match def {
                Def::Endian(x) => self.lift_endian(x)?,
                Def::Alignment(x) => self.lift_alignment(x)?,
                Def::Space(x) => self.lift_memory_region(x)?,
                Def::Token(x) => self.lift_bit_fields(x)?,
                Def::Varnode(x) => self.lift_regs(x)?,
                Def::VarnodeAttach(x) => self.lift_reg_map(x)?,
                Def::PCodeOp(x) => self.lift_pcode_op(x)?,
                Def::Constructor(x) => self.lift_scanner(x)?,
                Def::Macro(x) => self.lift_macro(x)?,
            }
        }
        Ok(())
    }

    fn lift_endian(
        &mut self,
        endian: Loc<ast::Endian>,
    ) -> Result<(), LiftError> {
        self.endian = endian.into_value();
        Ok(())
    }

    fn lift_alignment(
        &mut self,
        alignment: Loc<usize>,
    ) -> Result<(), LiftError> {
        self.alignment = alignment.into_value();
        Ok(())
    }

    fn lift_memory_region(
        &mut self,
        space: ast::Space,
    ) -> Result<(), LiftError> {
        let id: Id = space.id.into();
        let ident = *id.ident();
        let region = MemoryRegion {
            id,
            kind: space.kind,
            size: space.size.into_value(),
            word_size: space.word_size.into_value(),
            is_default: space.is_default,
        };
        let expr = self.pool.add(Expr::MemoryRegion(region));
        self.scope.insert(ident, expr, Types::MemoryRegion);
        Ok(())
    }

    fn lift_bit_fields(
        &mut self,
        token: ast::Token,
    ) -> Result<(), LiftError> {
        let bit_width = token.bit_width;
        for field in token.fields {
            let mut field = BitField::from(field);
            field.bit_width = bit_width.into_value();
            let ident = *field.id.ident();
            let expr = self.pool.add(Expr::BitField(field));
            self.scope.insert(ident, expr, Types::BitField);
        }
        Ok(())
    }

    fn lift_regs(
        &mut self,
        varnode: Loc<ast::Varnode>,
    ) -> Result<(), LiftError> {
        let varnode = varnode.into_value();
        for id in varnode.ids {
            let id: Id = id.into();
            let ident = *id.ident();
            let register = Register {
                id,
                size: varnode.byte_size.into_value(),
            };
            let expr = self.pool.add(Expr::Register(register));
            self.scope.insert(ident, expr, Types::Register);
        }
        Ok(())
    }

    fn lift_reg_map(
        &mut self,
        attach: Loc<ast::VarnodeAttach>,
    ) -> Result<(), LiftError> {
        let underscore = Ident::new("_");
        let attach = attach.into_value();
        let mut registers = Vec::new();
        // register map
        for id in attach.registers {
            let maybe_reg = if id.value() != &underscore {
                let reg = self.scope.lookup(&id, Types::Register)?;
                Some(reg)
            } else {
                None
            };
            registers.push(maybe_reg);
        }
        let register_map_idx = self.register_maps.len();
        self.register_maps.push(RegisterMap { registers });
        // register indices that refer to the map
        for id in attach.fields {
            let bit_field = self.scope.lookup(&id, Types::BitField)?;
            let index = RegisterIndex {
                register_map_idx,
                bit_field,
            };
            let expr = self.pool.add(Expr::RegisterIndex(index));
            self.scope
                .insert(id.into_value(), expr, Types::RegisterIndex);
        }
        Ok(())
    }

    fn lift_pcode_op(&mut self, id: Loc<Ident>) -> Result<(), LiftError> {
        let op = Intrinsic { id: id.into() };
        let expr = self.pool.add(Expr::Intrinsic(op));
        self.scope.insert(id.into_value(), expr, Types::Intrinsic);
        Ok(())
    }

    fn lift_macro(
        &mut self,
        r#macro: ast::Macro,
    ) -> Result<(), LiftError> {
        let mut scope = self.scope.to_local();
        let args = r#macro.args.into_iter().map(|id| id.into()).collect();
        let body: Result<Vec<ExprId>, _> = r#macro
            .body
            .into_iter()
            .map(|stmt| Expr::lift_stmt(&mut self.pool, &mut scope, stmt))
            .collect();
        let id: Id = r#macro.id.into();
        let ident = *id.ident();
        let r#macro = Macro {
            id,
            args,
            body: body?,
            scope,
        };
        let expr = self.pool.add(Expr::Macro(r#macro));
        self.scope.insert(ident, expr, Types::Macro);
        Ok(())
    }

    fn lift_scanner(
        &mut self,
        ctr: ast::Constructor,
    ) -> Result<(), LiftError> {
        let id: Id = ctr.id.into();
        let span = id.span();
        let ident = *id.ident();
        let is_instruction = ctr.is_instruction;
        let result = self.scope.lookup(&ctr.id, Types::Scanner).ok();
        let (scanner, existing) = match (is_instruction, result) {
            (false, Some(scanner)) => (scanner, true),
            _ => {
                let scanner = Scanner {
                    id,
                    is_instruction,
                    rules: Vec::new(),
                };
                let scanner = self.pool.add(Expr::Scanner(scanner));
                (scanner, false)
            }
        };
        {
            let rule =
                Rule::lift(&mut self.pool, self.scope.to_local(), ctr)?;
            let rule = self.pool.add(Expr::Rule(rule));
            if let Expr::Scanner(scanner) = &mut self.pool[&scanner] {
                scanner.rules.push(rule);
            } else {
                return Err(LiftError::InternalTypeMismatch(span));
            }
        }
        if !existing {
            self.scope.insert(ident, scanner, Types::Scanner);
        }
        Ok(())
    }
}

impl Default for Architecture {
    fn default() -> Self {
        let mut arch = Self {
            endian: Endian::Little,
            alignment: 4,
            scope: Scope::default(),
            default_region: None,
            register_maps: Vec::new(),
            pool: ExprPool::new(),
        };
        // const space
        let region_ident = Ident::new("const");
        let region_id = Id::new(region_ident);
        let region = MemoryRegion {
            id: region_id,
            kind: ast::SpaceKind::Rom,
            size: 0,
            word_size: 1,
            is_default: false,
        };
        let expr = arch.pool.add(Expr::MemoryRegion(region));
        arch.scope.insert(region_ident, expr, Types::MemoryRegion);
        INTRINSICS.iter().for_each(|name| {
            let ident = Ident::new(name);
            let id = Id::new(ident);
            let intrinsic = Intrinsic { id };
            let expr = arch.pool.add(Expr::Intrinsic(intrinsic));
            arch.scope.insert(ident, expr, Types::Intrinsic);
        });
        arch
    }
}

const INTRINSICS: [&str; 31] = [
    "inst_start",
    "inst_next",
    "zext",
    "sext",
    "carry",
    "scarry",
    "sborrow",
    "int2float",
    "float2float",
    "floor",
    "mfsr",
    "mtsr",
    "msync",
    "isync",
    "dpref",
    "dsb",
    "isb",
    "break",
    "syscall",
    "trap",
    "cctl",
    "setgie",
    "setend",
    "TLB_TargetRead",
    "TLB_TargetWrite",
    "TLB_RWrite",
    "TLB_RWriteLock",
    "TLB_Unlock",
    "TLB_Probe",
    "TLB_Invalidate",
    "TLB_FlushAll",
];
