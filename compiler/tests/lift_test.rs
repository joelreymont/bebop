use bebop_compiler::{env::*, error::*, hir::*};
use bebop_parser::{parse, *};
use bebop_util::{id::*, meta::*};
use insta::*;

fn parse_and_lift(s: &str) -> Result<Architecture, LiftError> {
    let ast =
        parse(DefsParserEx::new(), s).map_err(LiftError::ParserError)?;
    let mut arch = Architecture::new();
    arch.lift(ast)?;
    Ok(arch)
}

#[test]
fn lift_token() -> Result<(), LiftError> {
    let s = r"
        define token instr32(32)
            Rt          = (20, 24)
            Ra          = (20, 24) hex
            Rb          = (20, 24) signed
        ;
    ";
    let arch = parse_and_lift(s)?;
    assert_ron_snapshot!(arch, @r#"
    Architecture(
      endian: Endian(Little),
      alignment: Alignment(4),
      scope: Scope(
        env: TypeEnv(
          env: {
            ("const", Types("MemoryRegion")): MemoryRegion(MemoryRegion(
              id: MetaId("const"),
              kind: Rom,
              size: 0,
              word_size: 1,
              is_default: false,
            )),
            ("inst_start", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("inst_start"),
            )),
            ("inst_next", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("inst_next"),
            )),
            ("zext", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("zext"),
            )),
            ("sext", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("sext"),
            )),
            ("carry", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("carry"),
            )),
            ("scarry", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("scarry"),
            )),
            ("sborrow", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("sborrow"),
            )),
            ("int2float", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("int2float"),
            )),
            ("float2float", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("float2float"),
            )),
            ("floor", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("floor"),
            )),
            ("mfsr", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("mfsr"),
            )),
            ("mtsr", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("mtsr"),
            )),
            ("msync", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("msync"),
            )),
            ("isync", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("isync"),
            )),
            ("dpref", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("dpref"),
            )),
            ("dsb", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("dsb"),
            )),
            ("isb", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("isb"),
            )),
            ("break", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("break"),
            )),
            ("syscall", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("syscall"),
            )),
            ("trap", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("trap"),
            )),
            ("cctl", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("cctl"),
            )),
            ("setgie", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("setgie"),
            )),
            ("setend", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("setend"),
            )),
            ("TLB_TargetRead", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_TargetRead"),
            )),
            ("TLB_TargetWrite", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_TargetWrite"),
            )),
            ("TLB_RWrite", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_RWrite"),
            )),
            ("TLB_RWriteLock", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_RWriteLock"),
            )),
            ("TLB_Unlock", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_Unlock"),
            )),
            ("TLB_Probe", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_Probe"),
            )),
            ("TLB_Invalidate", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_Invalidate"),
            )),
            ("TLB_FlushAll", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_FlushAll"),
            )),
            ("Rt", Types("BitField")): BitField(BitField(
              id: MetaId("Rt"),
              bit_width: 32,
              start_bit: 20,
              end_bit: 24,
              is_signed: false,
              is_hex: false,
            )),
            ("Ra", Types("BitField")): BitField(BitField(
              id: MetaId("Ra"),
              bit_width: 32,
              start_bit: 20,
              end_bit: 24,
              is_signed: false,
              is_hex: true,
            )),
            ("Rb", Types("BitField")): BitField(BitField(
              id: MetaId("Rb"),
              bit_width: 32,
              start_bit: 20,
              end_bit: 24,
              is_signed: true,
              is_hex: false,
            )),
          },
        ),
      ),
      default_region: None,
      register_maps: [],
    )
    "#);
    Ok(())
}

#[test]
fn lift_ctr_simple() -> Result<(), LiftError> {
    let s = r"
        define token instr32(32)
            Rt          = (20, 24)
            Ra          = (10, 16)
        ;
        foo: Rt is unimpl
        foo: Ra is unimpl
    ";
    let arch = parse_and_lift(s)?;
    assert_ron_snapshot!(arch, @r#"
    Architecture(
      endian: Endian(Little),
      alignment: Alignment(4),
      scope: Scope(
        env: TypeEnv(
          env: {
            ("const", Types("MemoryRegion")): MemoryRegion(MemoryRegion(
              id: MetaId("const"),
              kind: Rom,
              size: 0,
              word_size: 1,
              is_default: false,
            )),
            ("inst_start", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("inst_start"),
            )),
            ("inst_next", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("inst_next"),
            )),
            ("zext", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("zext"),
            )),
            ("sext", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("sext"),
            )),
            ("carry", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("carry"),
            )),
            ("scarry", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("scarry"),
            )),
            ("sborrow", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("sborrow"),
            )),
            ("int2float", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("int2float"),
            )),
            ("float2float", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("float2float"),
            )),
            ("floor", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("floor"),
            )),
            ("mfsr", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("mfsr"),
            )),
            ("mtsr", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("mtsr"),
            )),
            ("msync", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("msync"),
            )),
            ("isync", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("isync"),
            )),
            ("dpref", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("dpref"),
            )),
            ("dsb", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("dsb"),
            )),
            ("isb", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("isb"),
            )),
            ("break", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("break"),
            )),
            ("syscall", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("syscall"),
            )),
            ("trap", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("trap"),
            )),
            ("cctl", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("cctl"),
            )),
            ("setgie", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("setgie"),
            )),
            ("setend", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("setend"),
            )),
            ("TLB_TargetRead", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_TargetRead"),
            )),
            ("TLB_TargetWrite", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_TargetWrite"),
            )),
            ("TLB_RWrite", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_RWrite"),
            )),
            ("TLB_RWriteLock", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_RWriteLock"),
            )),
            ("TLB_Unlock", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_Unlock"),
            )),
            ("TLB_Probe", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_Probe"),
            )),
            ("TLB_Invalidate", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_Invalidate"),
            )),
            ("TLB_FlushAll", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: MetaId("TLB_FlushAll"),
            )),
            ("Rt", Types("BitField")): BitField(BitField(
              id: MetaId("Rt"),
              bit_width: 32,
              start_bit: 20,
              end_bit: 24,
              is_signed: false,
              is_hex: false,
            )),
            ("Ra", Types("BitField")): BitField(BitField(
              id: MetaId("Ra"),
              bit_width: 32,
              start_bit: 10,
              end_bit: 16,
              is_signed: false,
              is_hex: false,
            )),
            ("foo", Types("Scanner")): Scanner(Scanner(
              id: MetaId("foo"),
              rules: [
                Rule(Rule(
                  id: MetaId("foo"),
                  mnemonic: [],
                  output: [
                    Expr(BitField(BitField(
                      id: MetaId("Rt"),
                      bit_width: 32,
                      start_bit: 20,
                      end_bit: 24,
                      is_signed: false,
                      is_hex: false,
                    ))),
                  ],
                  setup: [],
                  actions: [],
                  pattern: [],
                  scope: Scope(
                    env: TypeEnv(
                      env: {},
                    ),
                  ),
                )),
                Rule(Rule(
                  id: MetaId("foo"),
                  mnemonic: [],
                  output: [
                    Expr(BitField(BitField(
                      id: MetaId("Ra"),
                      bit_width: 32,
                      start_bit: 10,
                      end_bit: 16,
                      is_signed: false,
                      is_hex: false,
                    ))),
                  ],
                  setup: [],
                  actions: [],
                  pattern: [],
                  scope: Scope(
                    env: TypeEnv(
                      env: {},
                    ),
                  ),
                )),
              ],
              is_instruction: false,
            )),
          },
        ),
      ),
      default_region: None,
      register_maps: [],
    )
    "#);
    Ok(())
}

#[test]
fn expand_macro() -> Result<(), LiftError> {
    let s = r"
        macro add(dst, a, b)
        {
            local sum = a + b;
            dst = sum;
        }
    ";
    let arch = parse_and_lift(s)?;
    let span = Span::default();
    let id = MetaId::from("add");
    let dst = MetaId::from("foo");
    let expr = arch.scope.lookup(&id, Types::Macro)?;
    let dst = ExprPtr::new(Expr::Variable(Variable { id: dst }), span);
    let a = ExprPtr::new(Expr::Int(Loc::new(10, span)), span);
    let b = ExprPtr::new(Expr::Int(Loc::new(20, span)), span);
    let args = vec![dst, a, b];
    let body = expr.apply(|expr| {
        let r#macro: &Macro = expr.try_into()?;
        r#macro.expand(&args)
    })?;
    assert_ron_snapshot!(body, @r#"
    [
      Bind(
        lhs: Variable(Variable(
          id: MetaId("dst"),
        )),
        rhs: Variable(Variable(
          id: MetaId("foo"),
        )),
      ),
      Bind(
        lhs: Variable(Variable(
          id: MetaId("a"),
        )),
        rhs: Int(10),
      ),
      Bind(
        lhs: Variable(Variable(
          id: MetaId("b"),
        )),
        rhs: Int(20),
      ),
      Bind(
        lhs: Variable(Variable(
          id: MetaId("sum"),
        )),
        rhs: Binary(
          op: PLUS(Unsigned),
          lhs: Variable(Variable(
            id: MetaId("a"),
          )),
          rhs: Variable(Variable(
            id: MetaId("b"),
          )),
        ),
      ),
      Bind(
        lhs: Variable(Variable(
          id: MetaId("dst"),
        )),
        rhs: Variable(Variable(
          id: MetaId("sum"),
        )),
      ),
    ]
    "#);
    Ok(())
}
