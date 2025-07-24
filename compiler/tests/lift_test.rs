use bebop_compiler::{error::*, hir::*};
use bebop_parser::{parse, *};
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
      endian: Little,
      alignment: 4,
      scope: Scope(
        env: TypeEnv(
          env: {
            ("const", Types("MemoryRegion")): MemoryRegion(MemoryRegion(
              id: Id("const"),
              kind: Rom,
              size: 0,
              word_size: 1,
              is_default: false,
            )),
            ("inst_start", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("inst_start"),
            )),
            ("inst_next", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("inst_next"),
            )),
            ("zext", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("zext"),
            )),
            ("sext", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("sext"),
            )),
            ("carry", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("carry"),
            )),
            ("scarry", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("scarry"),
            )),
            ("sborrow", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("sborrow"),
            )),
            ("int2float", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("int2float"),
            )),
            ("float2float", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("float2float"),
            )),
            ("floor", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("floor"),
            )),
            ("mfsr", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("mfsr"),
            )),
            ("mtsr", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("mtsr"),
            )),
            ("msync", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("msync"),
            )),
            ("isync", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("isync"),
            )),
            ("dpref", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("dpref"),
            )),
            ("dsb", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("dsb"),
            )),
            ("isb", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("isb"),
            )),
            ("break", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("break"),
            )),
            ("syscall", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("syscall"),
            )),
            ("trap", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("trap"),
            )),
            ("cctl", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("cctl"),
            )),
            ("setgie", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("setgie"),
            )),
            ("setend", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("setend"),
            )),
            ("TLB_TargetRead", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_TargetRead"),
            )),
            ("TLB_TargetWrite", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_TargetWrite"),
            )),
            ("TLB_RWrite", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_RWrite"),
            )),
            ("TLB_RWriteLock", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_RWriteLock"),
            )),
            ("TLB_Unlock", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_Unlock"),
            )),
            ("TLB_Probe", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_Probe"),
            )),
            ("TLB_Invalidate", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_Invalidate"),
            )),
            ("TLB_FlushAll", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_FlushAll"),
            )),
            ("Rt", Types("BitField")): BitField(BitField(
              id: Id("Rt"),
              bit_width: 32,
              start_bit: 20,
              end_bit: 24,
              is_signed: false,
              is_hex: false,
            )),
            ("Ra", Types("BitField")): BitField(BitField(
              id: Id("Ra"),
              bit_width: 32,
              start_bit: 20,
              end_bit: 24,
              is_signed: false,
              is_hex: true,
            )),
            ("Rb", Types("BitField")): BitField(BitField(
              id: Id("Rb"),
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
      endian: Little,
      alignment: 4,
      scope: Scope(
        env: TypeEnv(
          env: {
            ("const", Types("MemoryRegion")): MemoryRegion(MemoryRegion(
              id: Id("const"),
              kind: Rom,
              size: 0,
              word_size: 1,
              is_default: false,
            )),
            ("inst_start", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("inst_start"),
            )),
            ("inst_next", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("inst_next"),
            )),
            ("zext", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("zext"),
            )),
            ("sext", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("sext"),
            )),
            ("carry", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("carry"),
            )),
            ("scarry", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("scarry"),
            )),
            ("sborrow", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("sborrow"),
            )),
            ("int2float", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("int2float"),
            )),
            ("float2float", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("float2float"),
            )),
            ("floor", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("floor"),
            )),
            ("mfsr", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("mfsr"),
            )),
            ("mtsr", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("mtsr"),
            )),
            ("msync", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("msync"),
            )),
            ("isync", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("isync"),
            )),
            ("dpref", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("dpref"),
            )),
            ("dsb", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("dsb"),
            )),
            ("isb", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("isb"),
            )),
            ("break", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("break"),
            )),
            ("syscall", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("syscall"),
            )),
            ("trap", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("trap"),
            )),
            ("cctl", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("cctl"),
            )),
            ("setgie", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("setgie"),
            )),
            ("setend", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("setend"),
            )),
            ("TLB_TargetRead", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_TargetRead"),
            )),
            ("TLB_TargetWrite", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_TargetWrite"),
            )),
            ("TLB_RWrite", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_RWrite"),
            )),
            ("TLB_RWriteLock", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_RWriteLock"),
            )),
            ("TLB_Unlock", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_Unlock"),
            )),
            ("TLB_Probe", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_Probe"),
            )),
            ("TLB_Invalidate", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_Invalidate"),
            )),
            ("TLB_FlushAll", Types("Intrinsic")): Intrinsic(Intrinsic(
              id: Id("TLB_FlushAll"),
            )),
            ("Rt", Types("BitField")): BitField(BitField(
              id: Id("Rt"),
              bit_width: 32,
              start_bit: 20,
              end_bit: 24,
              is_signed: false,
              is_hex: false,
            )),
            ("Ra", Types("BitField")): BitField(BitField(
              id: Id("Ra"),
              bit_width: 32,
              start_bit: 10,
              end_bit: 16,
              is_signed: false,
              is_hex: false,
            )),
            ("foo", Types("Scanner")): Scanner(Scanner(
              id: Id("foo"),
              rules: [
                Rule(Rule(
                  id: Id("foo"),
                  mnemonic: [],
                  output: [
                    Expr(BitField(BitField(
                      id: Id("Rt"),
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
                  id: Id("foo"),
                  mnemonic: [],
                  output: [
                    Expr(BitField(BitField(
                      id: Id("Ra"),
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
