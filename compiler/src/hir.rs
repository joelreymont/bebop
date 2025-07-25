use crate::env::*;
use crate::error::LiftError;
use bebop_parser::ast;
use bebop_util::meta::*;
use serde::{Serialize, Serializer};
use std::cell::RefCell;
use std::option::Option::*;
use std::rc::Rc;

pub type Ident = ast::Ident;
pub type Endian = ast::Endian;

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct Id(Tagged<Ident, Meta>);

impl Id {
    pub fn new(id: Ident) -> Self {
        Self(Tagged::new(id, Meta::default()))
    }

    pub fn ident(&self) -> &Ident {
        self.0.value()
    }

    pub fn span(&self) -> Span {
        self.0.tag().span()
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

#[derive(Debug, Copy, Clone, Default, PartialEq, Serialize)]
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

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum Hint {
    Unsigned,
    Signed,
    Float,
}

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

    pub fn apply<T>(
        &self,
        fun: impl Fn(&Expr) -> Result<T, LiftError>,
    ) -> Result<T, LiftError> {
        fun(&self.expr.borrow())
    }

    pub fn apply_mut(
        &mut self,
        mut fun: impl FnMut(&mut Expr) -> Result<(), LiftError>,
    ) -> Result<(), LiftError> {
        fun(&mut self.expr.borrow_mut())
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
        // let mut expr = serializer.serialize_struct("ExprPtr", 2)?;
        // expr.serialize_field("expr", &self.expr)?;
        // expr.serialize_field("span", &self.span)?;
        // expr.end()
    }
}

pub type LiftResult = Result<ExprPtr, LiftError>;

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
        id: Id,
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
    MacroCall {
        r#macro: ExprPtr,
        args: Vec<ExprPtr>,
    },
    Transfer(Transfer),
    Branch {
        condition: ExprPtr,
        target: JumpTarget,
    },
    Export(ExprPtr),
    Build(ExprPtr),
    Label(Id),
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
            TakeBits { id, .. } => id.span(),
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
            MacroCall { r#macro, .. } => r#macro.span(),
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
    pub fn lift(scope: &Scope, expr: &ast::Expr) -> LiftResult {
        use ast::Expr::*;
        match expr {
            Binary { op, lhs, rhs } => {
                let span = lhs.span();
                let lhs = Self::lift(scope, lhs)?;
                let rhs = Self::lift(scope, rhs)?;
                let op = BinaryOp::try_from(op)?;
                let expr = Expr::Binary { op, lhs, rhs };
                Ok(ExprPtr::new(expr, span))
            }
            Unary { op, rhs } => {
                let span = rhs.span();
                let rhs = Self::lift(scope, rhs)?;
                let op = UnaryOp::try_from(op)?;
                let expr = Expr::Unary { op, rhs };
                Ok(ExprPtr::new(expr, span))
            }
            Paren(expr) => {
                let span = expr.span();
                let id = Self::lift(scope, expr)?;
                let expr = Expr::Paren(id);
                Ok(ExprPtr::new(expr, span))
            }
            FunCall { id, args } => {
                let id: self::Id = id.into();
                let span = id.span();
                let intrinsic = scope.lookup(&id, Types::Intrinsic)?;
                let args: Result<Vec<ExprPtr>, _> = args
                    .iter()
                    .map(|arg| Self::lift(scope, arg))
                    .collect();
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
                let span = id.span();
                let expr = Expr::TakeBits {
                    id: id.into(),
                    start_bit: start_bit.into_value(),
                    bit_width: bit_width.into_value(),
                };
                Ok(ExprPtr::new(expr, span))
            }
            Sized { expr, size } => {
                let span = expr.span();
                let id = Self::lift(scope, expr)?;
                let expr = Expr::TakeBytes {
                    expr: id,
                    n: size.into_value(),
                };
                Ok(ExprPtr::new(expr, span))
            }
            Pointer { expr, space } => {
                let span = expr.span();
                let expr = Self::lift(scope, expr)?;
                let region = space
                    .map(|id| {
                        let id: self::Id = id.into();
                        scope.lookup(&id, Types::MemoryRegion)
                    })
                    .transpose()?;
                let expr = Expr::Pointer { expr, region };
                Ok(ExprPtr::new(expr, span))
            }
            Id(id) => {
                let id: self::Id = id.into();
                let span = id.span();
                let expr = scope.find(&id, Types::all())?;
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

    pub fn lift_stmt(
        scope: &mut Scope,
        stmt: ast::Statement,
    ) -> LiftResult {
        use ast::Statement::*;
        match stmt {
            Bind { lhs, rhs } => {
                let span = lhs.span();
                scope.add_local_vars(&lhs, None)?;
                let lhs = Self::lift(scope, &lhs)?;
                let rhs = Self::lift(scope, &rhs)?;
                let expr = Expr::Bind { lhs, rhs };
                Ok(ExprPtr::new(expr, span))
            }
            FunCall { id, args } => {
                let id: self::Id = id.into();
                let span = id.span();
                let intrinsic = scope.lookup(&id, Types::Intrinsic)?;
                let args: Result<Vec<ExprPtr>, _> = args
                    .into_iter()
                    .map(|arg| Self::lift(scope, &arg))
                    .collect();
                let expr = Expr::FunCall {
                    intrinsic,
                    args: args?,
                };
                Ok(ExprPtr::new(expr, span))
            }
            Goto(target) => {
                let span = target.span();
                let target = JumpTarget::lift(scope, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Goto,
                    target,
                };
                let expr = Expr::Transfer(xfer);
                Ok(ExprPtr::new(expr, span))
            }
            Call(target) => {
                let span = target.span();
                let target = JumpTarget::lift(scope, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Call,
                    target,
                };
                let expr = Expr::Transfer(xfer);
                Ok(ExprPtr::new(expr, span))
            }
            Return(target) => {
                let span = target.span();
                let target = JumpTarget::lift(scope, target)?;
                let xfer = Transfer {
                    kind: TransferKind::Return,
                    target,
                };
                let expr = Expr::Transfer(xfer);
                Ok(ExprPtr::new(expr, span))
            }
            Branch { condition, target } => {
                let span = condition.span();
                let condition = Expr::lift(scope, &condition)?;
                let target = JumpTarget::lift(scope, target)?;
                let expr = Expr::Branch { condition, target };
                Ok(ExprPtr::new(expr, span))
            }
            Export(expr) => {
                let span = expr.span();
                let expr = Expr::lift(scope, &expr)?;
                let expr = Expr::Export(expr);
                Ok(ExprPtr::new(expr, span))
            }
            Label(id) => {
                let span = id.span();
                let expr = Expr::Label(id.into());
                Ok(ExprPtr::new(expr, span))
            }
            Build(id) => {
                let id: self::Id = id.into();
                let span = id.span();
                let scanner = scope.lookup(&id, Types::Scanner)?;
                let scanner = scanner.with_span(span);
                let expr = Expr::Build(scanner);
                Ok(ExprPtr::new(expr, span))
            }
        }
    }

    pub fn macroexpand(
        _i: usize,
        _id: ExprPtr,
    ) -> Result<(usize, Vec<ExprPtr>), LiftError> {
        Ok((0, Vec::new()))
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
    pub registers: Vec<Option<ExprPtr>>,
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
    pub bit_field: ExprPtr,
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
    Direct(ExprPtr),
    Indirect(ExprPtr),
    Label(Loc<Ident>),
}

impl JumpTarget {
    pub fn lift(
        scope: &Scope,
        target: ast::JumpTarget,
    ) -> Result<Self, LiftError> {
        use ast::JumpTarget::*;
        let target = match target {
            Fixed { address, space } => {
                let region = space
                    .map(|id| {
                        let id: self::Id = id.into();
                        scope.lookup(&id, Types::MemoryRegion)
                    })
                    .transpose()?;
                let addr = Address { address, region };
                JumpTarget::Fixed(addr)
            }
            Direct(id) => {
                let id: self::Id = id.into();
                let expr = scope.find(&id, Types::all())?;
                JumpTarget::Direct(expr)
            }
            Indirect(expr) => {
                let expr = Expr::lift(scope, &expr)?;
                JumpTarget::Indirect(expr)
            }
            Label(id) => JumpTarget::Label(id), // TODO: Validate in this env!
        };
        Ok(target)
    }
}

impl Spanned for JumpTarget {
    fn span(&self) -> Span {
        use JumpTarget::*;
        match self {
            Fixed(_) => Span::default(), // TODO: Fix this!
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
    pub body: Vec<ExprPtr>,
    pub scope: Scope,
}

impl Macro {
    fn lift(
        scope: &mut Scope,
        r#macro: ast::Macro,
    ) -> Result<(), LiftError> {
        let mut local_scope = scope.to_local();
        let args: Vec<_> =
            r#macro.args.into_iter().map(|id| id.into()).collect();
        args.iter().for_each(|&id| {
            let expr = Expr::Variable(Variable { id });
            let expr = ExprPtr::new(expr, id.span());
            local_scope.insert(id, expr, Types::Variable);
        });
        let body: Result<Vec<ExprPtr>, _> = r#macro
            .body
            .into_iter()
            .map(|stmt| Expr::lift_stmt(&mut local_scope, stmt))
            .collect();
        let id: Id = r#macro.id.into();
        let span = id.span();
        let r#macro = Macro {
            id,
            args,
            body: body?,
            scope: local_scope,
        };
        let expr = ExprPtr::new(Expr::Macro(r#macro), span);
        scope.insert(id, expr, Types::Macro);
        Ok(())
    }

    pub fn expand(
        &self,
        args: &[ExprPtr],
    ) -> Result<Vec<ExprPtr>, LiftError> {
        if args.len() != self.args.len() {
            let span = if !args.is_empty() {
                args[0].span()
            } else {
                self.id.span()
            };
            return Err(LiftError::MacroArgumentMismatch(span));
        }
        let mut actions = Vec::new();
        for (id, value) in self.args.iter().zip(args.iter()) {
            let lhs = Expr::Variable(Variable { id: *id });
            let lhs = ExprPtr::new(lhs, value.span());
            let rhs = value.clone();
            let expr = Expr::Bind { lhs, rhs };
            let expr = ExprPtr::new(expr, value.span());
            actions.push(expr)
        }
        actions.extend(self.body.iter().cloned());
        Ok(actions)
    }
}

impl<'a> TryFrom<&'a Expr> for &'a Macro {
    type Error = LiftError;

    fn try_from(expr: &'a Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Macro(r#macro) => Ok(r#macro),
            _ => Err(LiftError::InternalTypeMismatch(expr.span())),
        }
    }
}

impl<'a> TryFrom<&'a mut Expr> for &'a mut Macro {
    type Error = LiftError;

    fn try_from(expr: &'a mut Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Macro(r#macro) => Ok(r#macro),
            _ => Err(LiftError::InternalTypeMismatch(expr.span())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Scanner {
    pub id: Id,
    pub rules: Vec<ExprPtr>,
    pub is_instruction: bool,
}

impl<'a> TryFrom<&'a mut Expr> for &'a mut Scanner {
    type Error = LiftError;

    fn try_from(expr: &'a mut Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Scanner(scanner) => Ok(scanner),
            _ => Err(LiftError::InternalTypeMismatch(expr.span())),
        }
    }
}

impl Scanner {
    pub fn macroexpand(&mut self) -> Result<(), LiftError> {
        for expr in self.rules.iter_mut() {
            expr.apply_mut(|expr| {
                let rule: &mut Rule = expr.try_into()?;
                rule.macroexpand()?;
                Ok(())
            })?;
        }
        Ok(())
    }
}

type Pattern = Vec<ExprPtr>;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Rule {
    pub id: Id,
    pub mnemonic: Vec<Output>,
    pub output: Vec<Output>,
    pub setup: Vec<ExprPtr>,
    pub actions: Vec<ExprPtr>,
    pub pattern: Pattern,
    pub scope: Scope,
}

impl<'a> TryFrom<&'a mut Expr> for &'a mut Rule {
    type Error = LiftError;

    fn try_from(expr: &'a mut Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Rule(rule) => Ok(rule),
            _ => Err(LiftError::InternalTypeMismatch(expr.span())),
        }
    }
}

impl Rule {
    pub fn lift(
        mut scope: Scope,
        ctr: ast::Constructor,
    ) -> Result<Self, LiftError> {
        let setup: Result<Vec<ExprPtr>, LiftError> = ctr
            .context
            .into_iter()
            .map(|stmt| Expr::lift_stmt(&mut scope, stmt))
            .collect();
        let pattern = Self::lift_pattern(&scope, ctr.pattern)?;
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
        let actions: Result<Vec<ExprPtr>, LiftError> = ctr
            .body
            .into_iter()
            .map(|stmt| Expr::lift_stmt(&mut scope, stmt))
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
        scope: &Scope,
        acc: &mut Pattern,
        expr: ast::Expr,
    ) -> Result<(), LiftError> {
        use ast::Expr::*;
        match expr {
            Binary { op, lhs, rhs }
                if op.value() == &ast::BinaryOp::JOIN =>
            {
                let lhs = Expr::lift(scope, &lhs)?;
                let rhs = Expr::lift(scope, &rhs)?;
                acc.push(lhs);
                acc.push(rhs);
            }
            expr => {
                let expr = Expr::lift(scope, &expr)?;
                acc.push(expr);
            }
        }
        Ok(())
    }

    fn lift_pattern(
        scope: &Scope,
        expr: Option<ast::Expr>,
    ) -> Result<Pattern, LiftError> {
        let mut pattern = Vec::new();
        expr.map(|expr| Self::lift_pat_expr(scope, &mut pattern, expr))
            .transpose()?;
        Ok(pattern)
    }

    pub fn macroexpand(&mut self) -> Result<(), LiftError> {
        // Look for macro calls
        // Merge macro scope into the local one
        // For each macro argument
        //    Bind arg id to arg value
        //    Prepend to a copy of macro statements
        // Insert augmented macro statements at the point of the macro call
        /*
        pub struct Macro {
            pub id: Id,
            pub args: Vec<Id>,
            pub body: Vec<ExprPtr>,
            pub scope: Scope,
        }
        */
        // self.actions.iter().enumerate().for_each(
        //     |(i, id)| match &mut pool[id] {
        //         Expr::Rule(rule) => rule.macroexpand(pool),
        //         _ => Err(LiftError::InternalTypeMismatch(id.span())),
        //     },
        // );
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Output {
    Text(Ident),
    Expr(ExprPtr),
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
                let id = id.into();
                let expr = scope.find(&id, Types::all())?;
                Some(Output::Expr(expr))
            }
        };
        Ok(out)
    }
}

#[derive(Debug, Serialize)]
pub struct Architecture {
    pub endian: Endian,
    pub alignment: usize,
    pub scope: Scope,
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
    ) -> Result<(), LiftError> {
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
                Def::Macro(x) => Macro::lift(&mut self.scope, x)?,
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
        let span = id.span();
        let region = MemoryRegion {
            id,
            kind: space.kind,
            size: space.size.into_value(),
            word_size: space.word_size.into_value(),
            is_default: space.is_default,
        };
        let expr = ExprPtr::new(Expr::MemoryRegion(region), span);
        self.scope.insert(id, expr, Types::MemoryRegion);
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
            let id = field.id;
            let span = field.id.span();
            let expr = ExprPtr::new(Expr::BitField(field), span);
            self.scope.insert(id, expr, Types::BitField);
        }
        Ok(())
    }

    fn lift_registers(
        &mut self,
        varnode: Loc<ast::Varnode>,
    ) -> Result<(), LiftError> {
        let varnode = varnode.into_value();
        for id in varnode.ids {
            let id: Id = id.into();
            let span = id.span();
            let register = Register {
                id,
                size: varnode.byte_size.into_value(),
            };
            let expr = ExprPtr::new(Expr::Register(register), span);
            self.scope.insert(id, expr, Types::Register);
        }
        Ok(())
    }

    fn lift_register_map(
        &mut self,
        attach: Loc<ast::VarnodeAttach>,
    ) -> Result<(), LiftError> {
        let underscore = Ident::new("_");
        let attach = attach.into_value();
        let mut registers = Vec::new();
        // register map
        for id in attach.registers {
            let maybe_reg = if id.value() != &underscore {
                let id = id.into();
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
            let id: Id = id.into();
            let span = id.span();
            let bit_field = self.scope.lookup(&id, Types::BitField)?;
            let index = RegisterIndex {
                register_map_idx,
                bit_field,
            };
            let expr = ExprPtr::new(Expr::RegisterIndex(index), span);
            self.scope.insert(id, expr, Types::RegisterIndex);
        }
        Ok(())
    }

    fn lift_pcode_op(&mut self, id: Loc<Ident>) -> Result<(), LiftError> {
        let id: Id = id.into();
        let span = id.span();
        let op = Intrinsic { id };
        let expr = ExprPtr::new(Expr::Intrinsic(op), span);
        self.scope.insert(id, expr, Types::Intrinsic);
        Ok(())
    }

    fn lift_scanner(
        &mut self,
        ctr: ast::Constructor,
    ) -> Result<(), LiftError> {
        let id: Id = ctr.id.into();
        let span = id.span();
        let is_instruction = ctr.is_instruction;
        let result = self.scope.lookup(&id, Types::Scanner).ok();
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
            let rule = Rule::lift(self.scope.to_local(), ctr)?;
            let rule = ExprPtr::new(Expr::Rule(rule), span);
            scanner.apply_mut(|expr| {
                let scanner: &mut Scanner = expr.try_into()?;
                scanner.rules.push(rule.clone());
                Ok(())
            })?;
        }
        if !existing {
            self.scope.insert(id, scanner, Types::Scanner);
        }
        Ok(())
    }

    pub fn macroexpand(&mut self) -> Result<(), LiftError> {
        let iter = self
            .scope
            .iter_mut()
            .filter(|((_, types), _)| *types == Types::Scanner)
            .map(|(_, expr)| expr);
        for expr in iter {
            expr.apply_mut(|expr| {
                let scanner: &mut Scanner = expr.try_into()?;
                scanner.macroexpand()?;
                Ok(())
            })?;
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
        };
        // const space
        let region_id = Id::new(Ident::new("const"));
        let region = MemoryRegion {
            id: region_id,
            kind: ast::SpaceKind::Rom,
            size: 0,
            word_size: 1,
            is_default: false,
        };
        let span = Span::default();
        let expr = ExprPtr::new(Expr::MemoryRegion(region), span);
        arch.scope.insert(region_id, expr, Types::MemoryRegion);
        INTRINSICS.iter().for_each(|name| {
            let id = Id::new(Ident::new(name));
            let intrinsic = Intrinsic { id };
            let expr = ExprPtr::new(Expr::Intrinsic(intrinsic), span);
            arch.scope.insert(id, expr, Types::Intrinsic);
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
