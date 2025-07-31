use crate::env::{Env, Kind};
use crate::error::Error;
use bebop_parser::ast;
use bebop_util::id::{Hint, Id, LocId, MetaId};
use bebop_util::meta::{Loc, Span, Spanned};
use serde::{Serialize, Serializer};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprPtr {
    expr: Rc<RefCell<Expr>>,
    span: Span,
}

impl ExprPtr {
    pub fn new(expr: Expr, span: Span) -> Self {
        Self {
            expr: Rc::new(RefCell::new(expr)),
            span,
        }
    }

    pub fn with_span(&self, span: Span) -> Self {
        Self {
            expr: self.expr.clone(),
            span,
        }
    }

    pub fn apply<T>(&self, fun: impl Fn(&Expr) -> T) -> T {
        fun(&self.expr.borrow())
    }

    pub fn apply_mut<T>(
        &mut self,
        mut fun: impl FnMut(&mut Expr) -> T,
    ) -> T {
        fun(&mut self.expr.borrow_mut())
    }

    pub fn deepcopy(&mut self, env: &mut Env) -> Self {
        let span = self.span();
        self.apply_mut(|expr| {
            let result = match expr {
                Expr::Variable(Variable { id }) => {
                    env.lookup(id, Kind::Variable).ok()
                }
                _ => None,
            };
            result.unwrap_or_else(|| {
                let ptr = Self {
                    expr: Rc::new(RefCell::new(expr.deepcopy(env))),
                    span,
                };
                if let Expr::Variable(Variable { id }) = expr {
                    env.insert(*id, ptr.clone(), Kind::Variable)
                }
                ptr
            })
        })
    }

    pub fn find_scanners(&mut self, env: &mut Env) -> Result<(), Error> {
        self.apply_mut(|x| x.find_scanners(env))
    }
}

impl Spanned for ExprPtr {
    fn span(&self) -> Span {
        self.span
    }
}

impl Serialize for ExprPtr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.expr.serialize(serializer)
    }
}

impl From<Expr> for ExprPtr {
    fn from(expr: Expr) -> Self {
        let span = expr.span();
        Self::new(expr, span)
    }
}

pub type LiftResult = Result<ExprPtr, Error>;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expr {
    Binary {
        op: BinaryOp,
        lhs: ExprPtr,
        rhs: ExprPtr,
    },
    Unary {
        op: UnaryOp,
        rhs: ExprPtr,
    },
    Paren(ExprPtr),
    Pointer {
        expr: ExprPtr,
        region: Option<ExprPtr>,
    },
    TakeBits {
        expr: ExprPtr,
        start_bit: usize,
        bit_width: usize,
    },
    TakeBytes {
        expr: ExprPtr,
        n: usize,
    },
    FunCall {
        intrinsic: ExprPtr,
        args: Vec<ExprPtr>,
    },
    Register(Register),
    RegisterIndex(RegisterIndex),
    BitField(BitField),
    Scanner(Scanner),
    Variable(Variable),
    Int(Loc<usize>),
    Unit(Loc<()>),
    // Statements
    Bind {
        lhs: ExprPtr,
        rhs: ExprPtr,
    },
    MacroCall(MacroCall),
    Transfer(Transfer),
    Branch {
        condition: ExprPtr,
        target: JumpTarget,
    },
    Export(ExprPtr),
    Build(ExprPtr),
    Label(MetaId),
    // Invalid expressions, for pooling only!
    MemoryRegion(MemoryRegion),
    Intrinsic(Intrinsic),
    Macro(Macro),
    Rule(Rule),
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        use Expr::*;
        match self {
            Binary { lhs, .. } => lhs.span(),
            Unary { rhs, .. } => rhs.span(),
            Paren(expr) => expr.span(),
            Pointer { expr, .. } => expr.span(),
            TakeBits { expr, .. } => expr.span(),
            TakeBytes { expr, .. } => expr.span(),
            FunCall { intrinsic, .. } => intrinsic.span(),
            Register(reg) => reg.id.span(),
            RegisterIndex(index) => index.bit_field.span(),
            BitField(field) => field.id.span(),
            Scanner(scanner) => scanner.id.span(),
            Variable(var) => var.id.span(),
            Int(loc) => *loc.tag(),
            Unit(loc) => *loc.tag(),
            Bind { lhs, .. } => lhs.span(),
            MacroCall(call) => call.r#macro.span(),
            Transfer(xfer) => xfer.span(),
            Branch { condition, .. } => condition.span(),
            Export(expr) => expr.span(),
            Build(expr) => expr.span(),
            Label(id) => id.span(),
            MemoryRegion(region) => region.id.span(),
            Intrinsic(intrinsic) => intrinsic.id.span(),
            Macro(r#macro) => r#macro.id.span(),
            Rule(rule) => rule.id.span(),
        }
    }
}

impl Expr {
    pub fn lift(env: &Env, expr: &ast::Expr) -> LiftResult {
        use ast::Expr::*;
        match expr {
            Binary { op, lhs, rhs } => {
                let span = lhs.span();
                let lhs = Self::lift(env, lhs)?;
                let rhs = Self::lift(env, rhs)?;
                let op = BinaryOp::try_from(op)?;
                let expr = Expr::Binary { op, lhs, rhs };
                Ok(ExprPtr::new(expr, span))
            }
            Unary { op, rhs } => {
                let span = rhs.span();
                let rhs = Self::lift(env, rhs)?;
                let op = UnaryOp::try_from(op)?;
                let expr = Expr::Unary { op, rhs };
                Ok(ExprPtr::new(expr, span))
            }
            Paren(expr) => {
                let span = expr.span();
                let id = Self::lift(env, expr)?;
                let expr = Expr::Paren(id);
                Ok(ExprPtr::new(expr, span))
            }
            FunCall { id, args } => {
                let id = MetaId::from(id);
                let span = id.span();
                let intrinsic = env.lookup(&id, Kind::Intrinsic)?;
                let args: Result<Vec<ExprPtr>, _> =
                    args.iter().map(|arg| Self::lift(env, arg)).collect();
                let expr = Expr::FunCall {
                    intrinsic,
                    args: args?,
                };
                Ok(ExprPtr::new(expr, span))
            }
            BitRange {
                id,
                start_bit,
                bit_width,
            } => {
                let id = MetaId::from(id);
                let span = id.span();
                let expr = env.find(&id, Kind::all())?;
                let expr = expr.with_span(span);
                let expr = Expr::TakeBits {
                    expr,
                    start_bit: start_bit.into_value(),
                    bit_width: bit_width.into_value(),
                };
                Ok(ExprPtr::new(expr, span))
            }
            Sized { expr, size } => {
                let span = expr.span();
                let id = Self::lift(env, expr)?;
                let expr = Expr::TakeBytes {
                    expr: id,
                    n: size.into_value(),
                };
                Ok(ExprPtr::new(expr, span))
            }
            Pointer { expr, space } => {
                let span = expr.span();
                let expr = Self::lift(env, expr)?;
                let region = space
                    .map(|id| {
                        let id = MetaId::from(id);
                        env.lookup(&id, Kind::MemoryRegion)
                    })
                    .transpose()?;
                let expr = Expr::Pointer { expr, region };
                Ok(ExprPtr::new(expr, span))
            }
            Id(id) => {
                let id = MetaId::from(id);
                let span = id.span();
                let expr = env.find(&id, Kind::all())?;
                Ok(expr.with_span(span))
            }
            Int(n) => {
                let span = n.span();
                let expr = Expr::Int(*n);
                Ok(ExprPtr::new(expr, span))
            }
            Unit(x) => Ok(ExprPtr::new(Expr::Unit(*x), x.span())),
        }
    }

    pub fn lift_stmt(env: &mut Env, stmt: ast::Statement) -> LiftResult {
        use ast::Statement::*;
        match stmt {
            Bind { lhs, rhs } => {
                let span = lhs.span();
                env.add_local_vars(&lhs, None)?;
                let lhs = Self::lift(env, &lhs)?;
                let rhs = Self::lift(env, &rhs)?;
                let expr = Expr::Bind { lhs, rhs };
                Ok(ExprPtr::new(expr, span))
            }
            FunCall { id, args } => {
                let id = MetaId::from(id);
                let span = id.span();
                let args: Result<Vec<ExprPtr>, _> = args
                    .into_iter()
                    .map(|arg| Self::lift(env, &arg))
                    .collect();
                let expr = if let Ok(intrinsic) =
                    env.lookup(&id, Kind::Intrinsic)
                {
                    Expr::FunCall {
                        intrinsic,
                        args: args?,
                    }
                } else {
                    let r#macro = env.lookup(&id, Kind::Macro)?;
                    let call = MacroCall {
                        r#macro,
                        args: args?,
                    };
                    Expr::MacroCall(call)
                };
                Ok(ExprPtr::new(expr, span))
            }
            Goto(target) => {
                let span = target.span();
                let target = JumpTarget::lift(env, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Goto,
                    target,
                };
                let expr = Expr::Transfer(xfer);
                Ok(ExprPtr::new(expr, span))
            }
            Call(target) => {
                let span = target.span();
                let target = JumpTarget::lift(env, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Call,
                    target,
                };
                let expr = Expr::Transfer(xfer);
                Ok(ExprPtr::new(expr, span))
            }
            Return(target) => {
                let span = target.span();
                let target = JumpTarget::lift(env, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Return,
                    target,
                };
                let expr = Expr::Transfer(xfer);
                Ok(ExprPtr::new(expr, span))
            }
            Branch { condition, target } => {
                let span = condition.span();
                let condition = Expr::lift(env, &condition)?;
                let target = JumpTarget::lift(env, target)?;
                let expr = Expr::Branch { condition, target };
                Ok(ExprPtr::new(expr, span))
            }
            Export(expr) => {
                let span = expr.span();
                let expr = Expr::lift(env, &expr)?;
                let expr = Expr::Export(expr);
                Ok(ExprPtr::new(expr, span))
            }
            Label(id) => {
                let span = id.span();
                let expr = Expr::Label(id.into());
                Ok(ExprPtr::new(expr, span))
            }
            Build(id) => {
                let id = MetaId::from(id);
                let span = id.span();
                let scanner = env.lookup(&id, Kind::Scanner)?;
                let scanner = scanner.with_span(span);
                let expr = Expr::Build(scanner);
                Ok(ExprPtr::new(expr, span))
            }
        }
    }

    pub fn deepcopy(&mut self, env: &mut Env) -> Self {
        use Expr::*;
        match self {
            Binary { op, lhs, rhs } => Binary {
                op: op.clone(),
                lhs: lhs.deepcopy(env),
                rhs: rhs.deepcopy(env),
            },
            Unary { op, rhs } => Unary {
                op: op.clone(),
                rhs: rhs.deepcopy(env),
            },
            Paren(expr) => Paren(expr.deepcopy(env)),
            Pointer { expr, region } => Pointer {
                expr: expr.deepcopy(env),
                region: region.clone(),
            },
            TakeBits {
                expr,
                start_bit,
                bit_width,
            } => TakeBits {
                expr: expr.deepcopy(env),
                start_bit: *start_bit,
                bit_width: *bit_width,
            },
            TakeBytes { expr, n } => TakeBytes {
                expr: expr.deepcopy(env),
                n: *n,
            },
            FunCall { intrinsic, args } => {
                let intrinsic = intrinsic.clone(); // Shallow copy!
                let args =
                    args.iter_mut().map(|arg| arg.deepcopy(env)).collect();
                FunCall { intrinsic, args }
            }
            Bind { lhs, rhs } => Bind {
                lhs: lhs.deepcopy(env),
                rhs: rhs.deepcopy(env),
            },
            MacroCall(call) => {
                let r#macro = call.r#macro.clone(); // Shallow copy!
                let args = call
                    .args
                    .iter_mut()
                    .map(|arg| arg.deepcopy(env))
                    .collect();
                let call = self::MacroCall { r#macro, args };
                MacroCall(call)
            }
            Transfer(xfer) => Transfer(xfer.deepcopy(env)),
            Branch { condition, target } => Branch {
                condition: condition.deepcopy(env),
                target: target.deepcopy(env),
            },
            Export(expr) => Export(expr.deepcopy(env)),
            Build(expr) => Build(expr.clone()), // Shallow copy!
            expr => expr.clone(),               // Shallow copy!
        }
    }

    pub fn rename_variable(
        &mut self,
        new: &Id,
        span: Span,
    ) -> Result<(), Error> {
        match self {
            Expr::Variable(Variable { id }) => {
                id.rename(new);
                Ok(())
            }
            _ => Err(Error::Rename(span)),
        }
    }

    pub fn splice(
        &mut self,
        value: &ExprPtr,
        when: &impl Fn(&Expr) -> bool,
    ) {
        use Expr::*;
        match self {
            Binary { lhs, rhs, .. } => {
                lhs.apply_mut(|x| x.splice(value, when));
                rhs.apply_mut(|x| x.splice(value, when));
                lhs.apply(when).then(|| *lhs = value.clone());
                rhs.apply(when).then(|| *rhs = value.clone());
            }
            Unary { rhs, .. } => {
                rhs.apply_mut(|x| x.splice(value, when));
                rhs.apply(when).then(|| *rhs = value.clone());
            }
            Paren(expr) => {
                expr.apply_mut(|x| x.splice(value, when));
                expr.apply(when).then(|| *expr = value.clone());
            }
            Pointer { expr, .. } => {
                expr.apply_mut(|x| x.splice(value, when));
                expr.apply(when).then(|| *expr = value.clone());
            }
            TakeBits { expr, .. } => {
                expr.apply_mut(|x| x.splice(value, when));
                expr.apply(when).then(|| *expr = value.clone());
            }
            TakeBytes { expr, .. } => {
                expr.apply_mut(|x| x.splice(value, when));
                expr.apply(when).then(|| *expr = value.clone());
            }
            FunCall { args, .. } => {
                args.iter_mut().for_each(|expr| {
                    expr.apply_mut(|x| x.splice(value, when));
                    expr.apply(when).then(|| *expr = value.clone());
                });
            }
            Bind { lhs, rhs } => {
                lhs.apply_mut(|x| x.splice(value, when));
                rhs.apply_mut(|x| x.splice(value, when));
                lhs.apply(when).then(|| *lhs = value.clone());
                rhs.apply(when).then(|| *rhs = value.clone());
            }
            MacroCall(self::MacroCall { args, .. }) => {
                args.iter_mut().for_each(|expr| {
                    expr.apply_mut(|x| x.splice(value, when));
                    expr.apply(when).then(|| *expr = value.clone());
                });
            }
            Transfer(self::Transfer { target, .. }) => {
                target.splice(value, when);
            }
            Branch { condition, target } => {
                condition.apply(when).then(|| *condition = value.clone());
                target.splice(value, when);
            }
            Export(expr) => {
                expr.apply(when).then(|| *expr = value.clone());
            }
            Build(expr) => {
                expr.apply(when).then(|| *expr = value.clone());
            }
            _ => {}
        }
    }

    pub fn find_scanners(&mut self, env: &mut Env) -> Result<(), Error> {
        use Expr::*;
        match self {
            Binary { lhs, rhs, .. } => {
                lhs.find_scanners(env)?;
                rhs.find_scanners(env)?;
            }
            Unary { rhs, .. } => rhs.find_scanners(env)?,
            Paren(expr) => expr.find_scanners(env)?,
            Pointer { expr, .. } => expr.find_scanners(env)?,
            TakeBits { expr, .. } => expr.find_scanners(env)?,
            TakeBytes { expr, .. } => expr.find_scanners(env)?,
            FunCall { args, .. } => {
                for arg in args {
                    arg.find_scanners(env)?;
                }
            }
            Bind { lhs, rhs } => {
                lhs.find_scanners(env)?;
                rhs.find_scanners(env)?;
            }
            MacroCall(call) => {
                for arg in &mut call.args {
                    arg.find_scanners(env)?;
                }
            }
            Transfer(xfer) => xfer.find_scanners(env)?,
            Branch { condition, target } => {
                condition.find_scanners(env)?;
                target.find_scanners(env)?;
            }
            Export(expr) => expr.find_scanners(env)?,
            Build(expr) => {
                let id = expr.apply(|x| {
                    let scanner: &self::Scanner = x.try_into()?;
                    Ok(scanner.id)
                })?;
                env.insert(id, expr.clone(), Kind::Scanner);
            }
            _ => {}
        }
        Ok(())
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
    type Error = Error;

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
            _ => return Err(Error::Invalid(*span)),
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
    type Error = Error;

    fn try_from(op: &Loc<ast::UnaryOp>) -> Result<Self, Self::Error> {
        let (op, span) = (op.value(), op.tag());
        use ast::UnaryOp::*;
        let op = match op {
            NOT => Self::NOT,
            INV => Self::INV,
            NEG => Self::NEG { is_float: false },
            FNEG => Self::NEG { is_float: true },
            _ => return Err(Error::Invalid(*span)),
        };
        Ok(op)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MemoryRegion {
    pub id: MetaId,
    pub kind: ast::SpaceKind,
    pub size: usize,
    pub word_size: usize,
    pub is_default: bool,
}

impl MemoryRegion {
    fn lift(env: &mut Env, space: ast::Space) -> Result<(), Error> {
        let id: MetaId = space.id.into();
        let span = id.span();
        let region = MemoryRegion {
            id,
            kind: space.kind,
            size: space.size.into_value(),
            word_size: space.word_size.into_value(),
            is_default: space.is_default,
        };
        let expr = ExprPtr::new(Expr::MemoryRegion(region), span);
        env.insert(id, expr, Kind::MemoryRegion);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Register {
    pub id: MetaId,
    pub size: usize,
}

impl Register {
    fn lift(
        env: &mut Env,
        varnode: Loc<ast::Varnode>,
    ) -> Result<(), Error> {
        let varnode = varnode.into_value();
        for id in varnode.ids {
            let id: MetaId = id.into();
            let span = id.span();
            let register = Register {
                id,
                size: varnode.byte_size.into_value(),
            };
            let expr = ExprPtr::new(Expr::Register(register), span);
            env.insert(id, expr, Kind::Register);
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct RegisterMap {
    pub registers: Vec<Option<ExprPtr>>,
}

impl RegisterMap {
    fn lift(
        env: &mut Env,
        register_maps: &mut Vec<RegisterMap>,
        attach: Loc<ast::VarnodeAttach>,
    ) -> Result<(), Error> {
        let underscore = Id::new("_");
        let attach = attach.into_value();
        let mut registers = Vec::new();
        // register map
        for id in attach.registers {
            let maybe_reg = if id.id() != &underscore {
                let id = id.into();
                let reg = env.lookup(&id, Kind::Register)?;
                Some(reg)
            } else {
                None
            };
            registers.push(maybe_reg);
        }
        let register_map_idx = register_maps.len();
        register_maps.push(RegisterMap { registers });
        // register indices that refer to the map
        for id in attach.fields {
            let id: MetaId = id.into();
            let span = id.span();
            let bit_field = env.lookup(&id, Kind::BitField)?;
            let index = RegisterIndex {
                register_map_idx,
                bit_field,
            };
            let expr = ExprPtr::new(Expr::RegisterIndex(index), span);
            env.insert(id, expr, Kind::RegisterIndex);
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BitField {
    pub id: MetaId,
    pub bit_width: usize,
    pub start_bit: usize,
    pub end_bit: usize,
    pub is_signed: bool,
    pub is_hex: bool,
}

impl BitField {
    fn lift(env: &mut Env, token: ast::Token) -> Result<(), Error> {
        let bit_width = token.bit_width;
        for field in token.fields {
            let mut field = BitField::from(field);
            field.bit_width = bit_width.into_value();
            let id = field.id;
            let span = field.id.span();
            let expr = ExprPtr::new(Expr::BitField(field), span);
            env.insert(id, expr, Kind::BitField);
        }
        Ok(())
    }
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
    pub bit_field: ExprPtr,
    pub register_map_idx: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Variable {
    pub id: MetaId,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Intrinsic {
    pub id: MetaId,
}

impl Intrinsic {
    fn lift(env: &mut Env, id: LocId) -> Result<(), Error> {
        let id = MetaId::from(id);
        let span = id.span();
        let op = Intrinsic { id };
        let expr = ExprPtr::new(Expr::Intrinsic(op), span);
        env.insert(id, expr, Kind::Intrinsic);
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum JumpTarget {
    Fixed(Address),
    Direct(ExprPtr),
    Indirect(ExprPtr),
    Label(LocId),
}

impl JumpTarget {
    pub fn lift(
        env: &Env,
        target: ast::JumpTarget,
    ) -> Result<Self, Error> {
        use ast::JumpTarget::*;
        let target = match target {
            Fixed { address, space } => {
                let region = space
                    .map(|id| {
                        let id = MetaId::from(id);
                        env.lookup(&id, Kind::MemoryRegion)
                    })
                    .transpose()?;
                let addr = Address { address, region };
                JumpTarget::Fixed(addr)
            }
            Direct(id) => {
                let id = MetaId::from(id);
                let expr = env.find(&id, Kind::all())?;
                JumpTarget::Direct(expr)
            }
            Indirect(expr) => {
                let expr = Expr::lift(env, &expr)?;
                JumpTarget::Indirect(expr)
            }
            Label(id) => JumpTarget::Label(id), // TODO: Validate in this env!
        };
        Ok(target)
    }

    pub fn deepcopy(&mut self, env: &mut Env) -> Self {
        use JumpTarget::*;
        match self {
            Direct(expr) => Direct(expr.deepcopy(env)),
            Indirect(expr) => Indirect(expr.deepcopy(env)),
            target => target.clone(), // Shallow copy!
        }
    }

    pub fn splice(
        &mut self,
        value: &ExprPtr,
        when: &impl Fn(&Expr) -> bool,
    ) {
        use JumpTarget::*;
        match self {
            Direct(expr) => expr.apply_mut(|x| x.splice(value, when)),
            Indirect(expr) => expr.apply_mut(|x| x.splice(value, when)),
            _ => {}
        }
    }

    pub fn find_scanners(&mut self, env: &mut Env) -> Result<(), Error> {
        use JumpTarget::*;
        match self {
            Direct(expr) => expr.find_scanners(env)?,
            Indirect(expr) => expr.find_scanners(env)?,
            _ => {}
        }
        Ok(())
    }
}

impl Spanned for JumpTarget {
    fn span(&self) -> Span {
        use JumpTarget::*;
        match self {
            Fixed(address) => address.span(),
            Direct(expr) => expr.span(),
            Indirect(expr) => expr.span(),
            Label(id) => id.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Address {
    pub address: Loc<usize>,
    pub region: Option<ExprPtr>,
}

impl Spanned for Address {
    fn span(&self) -> Span {
        self.address.span()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Transfer {
    pub kind: TransferKind,
    pub target: JumpTarget,
}

impl Spanned for Transfer {
    fn span(&self) -> Span {
        self.target.span()
    }
}

impl Transfer {
    pub fn deepcopy(&mut self, env: &mut Env) -> Self {
        Self {
            kind: self.kind.clone(),
            target: self.target.deepcopy(env),
        }
    }

    pub fn find_scanners(&mut self, env: &mut Env) -> Result<(), Error> {
        self.target.find_scanners(env)
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
    pub id: MetaId,
    pub args: Vec<MetaId>,
    pub actions: Vec<ExprPtr>,
    pub env: Env,
}

impl Macro {
    fn lift(env: &mut Env, r#macro: ast::Macro) -> Result<(), Error> {
        let mut local_env = env.to_local();
        let args: Vec<_> =
            r#macro.args.into_iter().map(|id| id.into()).collect();
        args.iter().for_each(|&id| {
            let expr = Expr::Variable(Variable { id });
            let expr = ExprPtr::new(expr, id.span());
            local_env.insert(id, expr, Kind::Variable);
        });
        let actions: Result<Vec<ExprPtr>, _> = r#macro
            .body
            .into_iter()
            .map(|stmt| Expr::lift_stmt(&mut local_env, stmt))
            .collect();
        let id: MetaId = r#macro.id.into();
        let span = id.span();
        let r#macro = Macro {
            id,
            args,
            actions: actions?,
            env: local_env,
        };
        let expr = ExprPtr::new(Expr::Macro(r#macro), span);
        env.insert(id, expr, Kind::Macro);
        Ok(())
    }

    pub fn expand(
        &mut self,
        args: &[ExprPtr],
    ) -> Result<Vec<ExprPtr>, Error> {
        if args.len() != self.args.len() {
            let span = if !args.is_empty() {
                args[0].span()
            } else {
                self.id.span()
            };
            return Err(Error::MacroArgumentMismatch(span));
        }
        let mut actions = Vec::new();
        let mut env = Env::default();
        for action in &mut self.actions {
            let mut action =
                action.apply_mut(|expr| expr.deepcopy(&mut env));
            for (name, value) in self.args.iter().zip(args.iter()) {
                action.splice(value, &|expr| {
                    matches!(
                        expr,
                        Expr::Variable(Variable { id }) if name == id
                    )
                });
            }
            actions.push(ExprPtr::from(action));
        }
        Ok(actions)
    }
}

impl<'a> TryFrom<&'a Expr> for &'a Macro {
    type Error = Error;

    fn try_from(expr: &'a Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Macro(r#macro) => Ok(r#macro),
            _ => Err(Error::InternalTypeMismatch(expr.span())),
        }
    }
}

impl<'a> TryFrom<&'a mut Expr> for &'a mut Macro {
    type Error = Error;

    fn try_from(expr: &'a mut Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Macro(r#macro) => Ok(r#macro),
            _ => Err(Error::InternalTypeMismatch(expr.span())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MacroCall {
    pub r#macro: ExprPtr,
    pub args: Vec<ExprPtr>,
}

impl<'a> TryFrom<&'a Expr> for &'a MacroCall {
    type Error = Error;

    fn try_from(expr: &'a Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::MacroCall(call) => Ok(call),
            _ => Err(Error::InternalTypeMismatch(expr.span())),
        }
    }
}

impl<'a> TryFrom<&'a mut Expr> for &'a mut MacroCall {
    type Error = Error;

    fn try_from(expr: &'a mut Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::MacroCall(call) => Ok(call),
            _ => Err(Error::InternalTypeMismatch(expr.span())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Scanner {
    pub id: MetaId,
    pub rules: Vec<ExprPtr>,
    pub is_instruction: bool,
}

impl<'a> TryFrom<&'a Expr> for &'a Scanner {
    type Error = Error;

    fn try_from(expr: &'a Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Scanner(scanner) => Ok(scanner),
            _ => Err(Error::InternalTypeMismatch(expr.span())),
        }
    }
}

impl<'a> TryFrom<&'a mut Expr> for &'a mut Scanner {
    type Error = Error;

    fn try_from(expr: &'a mut Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Scanner(scanner) => Ok(scanner),
            _ => Err(Error::InternalTypeMismatch(expr.span())),
        }
    }
}

impl Scanner {
    fn lift(env: &mut Env, ctr: ast::Constructor) -> Result<(), Error> {
        let id: MetaId = ctr.id.into();
        let span = id.span();
        let is_instruction = ctr.is_instruction;
        let result = env.lookup(&id, Kind::Scanner).ok();
        let (mut scanner, existing) = match (is_instruction, result) {
            (false, Some(scanner)) => (scanner, true),
            _ => {
                let scanner = Scanner {
                    id,
                    is_instruction,
                    rules: Vec::new(),
                };
                let scanner = ExprPtr::new(Expr::Scanner(scanner), span);
                (scanner, false)
            }
        };
        {
            let rule = Rule::lift(env.to_local(), ctr)?;
            let rule = ExprPtr::new(Expr::Rule(rule), span);
            scanner.apply_mut(|expr| {
                let scanner: &mut Scanner = expr.try_into()?;
                scanner.rules.push(rule.clone());
                Ok(())
            })?;
        }
        if !existing {
            env.insert(id, scanner, Kind::Scanner);
        }
        Ok(())
    }

    pub fn macroexpand(&mut self) -> Result<(), Error> {
        for expr in &mut self.rules {
            expr.apply_mut(|expr| {
                let rule: &mut Rule = expr.try_into()?;
                rule.macroexpand()?;
                Ok(())
            })?;
        }
        Ok(())
    }

    pub fn inline_scanners(&mut self) -> Result<(), Error> {
        let mut new_rules = Vec::new();
        for expr in &mut self.rules {
            expr.apply_mut(|expr| {
                let rule: &mut Rule = expr.try_into()?;
                rule.inline_scanners(&mut new_rules)?;
                Ok(())
            })?;
        }
        self.rules = new_rules
            .into_iter()
            .map(|rule| {
                let span = rule.id.span();
                ExprPtr::new(Expr::Rule(rule), span)
            })
            .collect();
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Rule {
    pub id: MetaId,
    pub mnemonic: Vec<Output>,
    pub output: Vec<Output>,
    pub setup: Vec<ExprPtr>,
    pub actions: Vec<ExprPtr>,
    pub pattern: Option<ExprPtr>,
    pub env: Env,
}

impl<'a> TryFrom<&'a Expr> for &'a Rule {
    type Error = Error;

    fn try_from(expr: &'a Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Rule(rule) => Ok(rule),
            _ => Err(Error::InternalTypeMismatch(expr.span())),
        }
    }
}

impl<'a> TryFrom<&'a mut Expr> for &'a mut Rule {
    type Error = Error;

    fn try_from(expr: &'a mut Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Rule(rule) => Ok(rule),
            _ => Err(Error::InternalTypeMismatch(expr.span())),
        }
    }
}

impl Rule {
    pub fn lift(
        mut env: Env,
        ctr: ast::Constructor,
    ) -> Result<Self, Error> {
        let setup: Result<Vec<ExprPtr>, Error> = ctr
            .context
            .into_iter()
            .map(|stmt| Expr::lift_stmt(&mut env, stmt))
            .collect();
        let pattern = Self::lift_pattern(&env, ctr.pattern)?;
        let mnemonic: Vec<Output> = ctr
            .display
            .mnemonic
            .into_iter()
            .map(|piece| Output::lift(&env, piece))
            .collect::<Result<Vec<Option<Output>>, Error>>()?
            .into_iter()
            .flatten()
            .collect();
        let output: Vec<Output> = ctr
            .display
            .output
            .into_iter()
            .map(|piece| Output::lift(&env, piece))
            .collect::<Result<Vec<Option<Output>>, Error>>()?
            .into_iter()
            .flatten()
            .collect();
        let actions: Result<Vec<ExprPtr>, Error> = ctr
            .body
            .into_iter()
            .map(|stmt| Expr::lift_stmt(&mut env, stmt))
            .collect();
        let rule = Self {
            id: ctr.id.into(),
            mnemonic,
            output,
            setup: setup?,
            actions: actions?,
            pattern,
            env,
        };
        Ok(rule)
    }

    fn lift_pat_expr(env: &Env, expr: ast::Expr) -> LiftResult {
        use ast::Expr::*;
        match expr {
            Binary { ref op, .. }
                if op.value() == &ast::BinaryOp::JOIN =>
            {
                // TODO: Fix this!
                return Err(Error::VariableLengthPattern(expr.span()));
            }
            /*
            {
                let lhs = Expr::lift(env, &lhs)?;
                let rhs = Expr::lift(env, &rhs)?;
                acc.push(lhs);
                acc.push(rhs);
            }
            */
            expr => Expr::lift(env, &expr),
        }
    }

    fn lift_pattern(
        env: &Env,
        expr: Option<ast::Expr>,
    ) -> Result<Option<ExprPtr>, Error> {
        expr.map(|expr| Self::lift_pat_expr(env, expr)).transpose()
    }

    pub fn macroexpand(&mut self) -> Result<(), Error> {
        let result: Result<Vec<(usize, Vec<ExprPtr>)>, Error> = self
            .actions
            .iter_mut()
            .enumerate()
            .filter(|(_, expr)| {
                expr.apply(|x| matches!(x, Expr::MacroCall(_)))
            })
            .map(|(i, expr)| {
                expr.apply_mut(|x| {
                    let call: &mut MacroCall = x.try_into()?;
                    call.r#macro.apply_mut(|x| {
                        let r#macro: &mut Macro = x.try_into()?;
                        let actions = r#macro.expand(&call.args)?;
                        Ok((i, actions))
                    })
                })
            })
            .collect();
        let mut items = result?;
        for (_, actions) in &mut items {
            let mut env = self.env.empty();
            for expr in actions {
                env.import(expr)?;
            }
            env.make_unique()?;
            self.env.merge(&env);
        }
        let mut added_count = 0;
        for (i, actions) in items {
            let ix = i + added_count;
            added_count += actions.len() - 1;
            self.actions.splice(ix..ix + 1, actions.into_iter());
        }
        Ok(())
    }

    pub fn find_scanners(&mut self) -> Result<Env, Error> {
        let mut env = Env::default();
        for out in &mut self.output {
            out.find_scanners(&mut env)?;
        }
        if let Some(pat) = &mut self.pattern {
            pat.find_scanners(&mut env)?;
        }
        for action in &mut self.actions {
            action.find_scanners(&mut env)?;
        }
        Ok(env)
    }

    pub fn inline_scanners(
        &mut self,
        acc: &mut Vec<Rule>,
    ) -> Result<(), Error> {
        // inline each scanner
        self.find_scanners()?.apply_iter_mut(|iter| {
            let mut rules = vec![self.clone()];
            let scanners = iter
                .filter(|(k, _)| *k.kind() == Kind::Scanner)
                .map(|(_, v)| v.expr_mut());
            for scanner_expr in scanners {
                let scanner_expr1 = scanner_expr.clone();
                scanner_expr.apply_mut(|x| {
                    let scanner: &mut Scanner = x.try_into()?;
                    // which may have several rules
                    for scanner_rule in &mut scanner.rules {
                        scanner_rule.apply_mut(|x| {
                            let scanner_rule: &mut Rule = x.try_into()?;
                            // we may get several new rules back, e.g.
                            // if the scanner we are inlining itself has
                            // multiple rules
                            let mut new_rules = Vec::new();
                            // so each subsequent scanner will iterate
                            // over the rules we have collected so far
                            for rule in &mut rules {
                                rule.inline_rule(
                                    &mut new_rules,
                                    &scanner_expr1,
                                    scanner_rule,
                                )?;
                            }
                            // save new rules generated by inlining one scanner
                            rules.extend(new_rules);
                            Ok(())
                        })?;
                    }
                    Ok(())
                })?;
            }
            // save new rules generated by inlining all scanners
            acc.extend(rules);
            Ok(())
        })
    }

    pub fn inline_rule(
        &mut self,
        _acc: &mut Vec<Rule>,
        scanner: &ExprPtr,
        scanner_rule: &Rule,
    ) -> Result<(), Error> {
        // inline mnemonic
        let indices = self
            .mnemonic
            .iter()
            .enumerate()
            .filter(
                |(_, out)| matches!(out, Output::Expr(x) if x == scanner),
            )
            .map(|(i, _)| i)
            .collect::<Vec<_>>();
        for i in indices {
            self.mnemonic
                .splice(i..i + 1, scanner_rule.mnemonic.clone());
        }
        // inline output
        let indices = self
            .output
            .iter()
            .enumerate()
            .filter(
                |(_, out)| matches!(out, Output::Expr(x) if x == scanner),
            )
            .map(|(i, _)| i)
            .collect::<Vec<_>>();
        for i in indices {
            self.output.splice(i..i + 1, scanner_rule.output.clone());
        }
        // inline pattern
        match (&mut self.pattern, &scanner_rule.pattern) {
            (_, None) => {},
            (None, Some(pat)) => self.pattern = Some(pat.clone()),
            (Some(old), Some(new)) => {
                old.apply_mut(|pat| {
                    pat.splice(new, &|expr| {
                        matches!(
                            expr,
                            Expr::Scanner(Scanner{ id,.. }) if id == &scanner_rule.id
                        )
                    });
                })
            },
        }
        // inline actions
        // last statement -- export, use this as our splice value/
        /*
        let indices
        for action in &mut self.actions {
                old.apply_mut(|pat| {
                    pat.splice(new, &|expr| {
                        matches!(
                            expr,
                            Expr::Scanner(Scanner{ id,.. }) if id == &scanner_rule.id
                        )
                    });
                })

        }
                if Some(old, new) = self.pattern.zip(scanner.rule) {
                    pattern.splice(scanner.rule, &|expr| {
                        matches!(
                            expr,
                            Expr::Variable(Variable { id }) if name == id
                        )
                    });
                }
                // pub actions: Vec<ExprPtr>,
                // pub pattern: Pattern,
                for pat in &mut self.pattern {
                    pat.find_scanners(&mut env)?;
                }
                for action in &mut self.actions {
                    action.find_scanners(&mut env)?;
                }
        */
        // inline scanners in the resulting rule
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Output {
    Text(Id),
    Expr(ExprPtr),
}

impl Output {
    pub fn lift(
        env: &Env,
        piece: ast::DisplayPiece,
    ) -> Result<Option<Self>, Error> {
        use ast::DisplayPiece::*;
        let out = match piece {
            Text(id) => Some(Output::Text(id)),
            Caret | Space => None,
            Id(id) => {
                let id = id.into();
                let expr = env.find(&id, Kind::all())?;
                Some(Output::Expr(expr))
            }
        };
        Ok(out)
    }

    pub fn find_scanners(&mut self, env: &mut Env) -> Result<(), Error> {
        if let Self::Expr(expr) = self {
            expr.apply_mut(|x| x.find_scanners(env))?;
        }
        Ok(())
    }
}

#[derive(Debug, Serialize)]
pub struct Alignment(Loc<usize>);

impl Alignment {
    fn lift(value: Loc<usize>) -> Result<Self, Error> {
        Ok(Self(value))
    }
}

impl Default for Alignment {
    fn default() -> Self {
        Self(Loc::new(4, Span::default()))
    }
}

#[derive(Debug, Serialize)]
pub struct Endian(Loc<ast::Endian>);

impl Default for Endian {
    fn default() -> Self {
        Self(Loc::new(ast::Endian::Little, Span::default()))
    }
}

impl Endian {
    fn lift(value: Loc<ast::Endian>) -> Result<Self, Error> {
        Ok(Self(value))
    }
}

#[derive(Debug, Serialize)]
pub struct Architecture {
    pub endian: Endian,
    pub alignment: Alignment,
    pub env: Env,
    pub default_region: Option<ExprPtr>,
    pub register_maps: Vec<RegisterMap>,
}

impl Architecture {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn lift(
        &mut self,
        defs: Vec<ast::Definition>,
    ) -> Result<(), Error> {
        use ast::Definition as D;
        let env = &mut self.env;
        let reg_maps = &mut self.register_maps;
        for def in defs {
            match def {
                D::Endian(x) => self.endian = Endian::lift(x)?,
                D::Alignment(x) => self.alignment = Alignment::lift(x)?,
                D::Space(x) => MemoryRegion::lift(env, x)?,
                D::Token(x) => BitField::lift(env, x)?,
                D::Varnode(x) => Register::lift(env, x)?,
                D::VarnodeAttach(x) => {
                    RegisterMap::lift(env, reg_maps, x)?
                }
                D::PCodeOp(x) => Intrinsic::lift(env, x)?,
                D::Constructor(x) => Scanner::lift(env, x)?,
                D::Macro(x) => Macro::lift(env, x)?,
            }
        }
        Ok(())
    }

    pub fn macroexpand(&mut self) -> Result<(), Error> {
        self.env.apply_iter_mut(|iter| {
            let iter = iter
                .filter(|(k, _)| *k.kind() == Kind::Scanner)
                .map(|(_, v)| v.expr_mut());
            for expr in iter {
                expr.apply_mut(|expr| {
                    let scanner: &mut Scanner = expr.try_into()?;
                    scanner.macroexpand()?;
                    Ok(())
                })?;
            }
            Ok(())
        })
    }

    pub fn inline_scanners(&mut self) -> Result<(), Error> {
        self.env.apply_iter_mut(|iter| {
            let iter = iter
                .filter(|(k, _)| *k.kind() == Kind::Scanner)
                .map(|(_, v)| v.expr_mut());
            for expr in iter {
                expr.apply_mut(|expr| {
                    let scanner: &mut Scanner = expr.try_into()?;
                    scanner.inline_scanners()?;
                    Ok(())
                })?;
            }
            Ok(())
        })
    }
}

impl Default for Architecture {
    fn default() -> Self {
        let mut arch = Self {
            endian: Endian::default(),
            alignment: Alignment::default(),
            env: Env::default(),
            default_region: None,
            register_maps: Vec::new(),
        };
        // const space
        let region_id = MetaId::from("const");
        let region = MemoryRegion {
            id: region_id,
            kind: ast::SpaceKind::Rom,
            size: 0,
            word_size: 1,
            is_default: false,
        };
        let span = Span::default();
        let expr = ExprPtr::new(Expr::MemoryRegion(region), span);
        arch.env.insert(region_id, expr, Kind::MemoryRegion);
        INTRINSICS.iter().for_each(|name| {
            let id = MetaId::from(name);
            let intrinsic = Intrinsic { id };
            let expr = ExprPtr::new(Expr::Intrinsic(intrinsic), span);
            arch.env.insert(id, expr, Kind::Intrinsic);
        });
        arch
    }
}

const INTRINSICS: [&str; 10] = [
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
];
