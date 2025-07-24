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
            "const": [
              (Types("MemoryRegion"), 0),
            ],
            "inst_start": [
              (Types("Intrinsic"), 1),
            ],
            "inst_next": [
              (Types("Intrinsic"), 2),
            ],
            "zext": [
              (Types("Intrinsic"), 3),
            ],
            "sext": [
              (Types("Intrinsic"), 4),
            ],
            "carry": [
              (Types("Intrinsic"), 5),
            ],
            "scarry": [
              (Types("Intrinsic"), 6),
            ],
            "sborrow": [
              (Types("Intrinsic"), 7),
            ],
            "int2float": [
              (Types("Intrinsic"), 8),
            ],
            "float2float": [
              (Types("Intrinsic"), 9),
            ],
            "floor": [
              (Types("Intrinsic"), 10),
            ],
            "mfsr": [
              (Types("Intrinsic"), 11),
            ],
            "mtsr": [
              (Types("Intrinsic"), 12),
            ],
            "msync": [
              (Types("Intrinsic"), 13),
            ],
            "isync": [
              (Types("Intrinsic"), 14),
            ],
            "dpref": [
              (Types("Intrinsic"), 15),
            ],
            "dsb": [
              (Types("Intrinsic"), 16),
            ],
            "isb": [
              (Types("Intrinsic"), 17),
            ],
            "break": [
              (Types("Intrinsic"), 18),
            ],
            "syscall": [
              (Types("Intrinsic"), 19),
            ],
            "trap": [
              (Types("Intrinsic"), 20),
            ],
            "cctl": [
              (Types("Intrinsic"), 21),
            ],
            "setgie": [
              (Types("Intrinsic"), 22),
            ],
            "setend": [
              (Types("Intrinsic"), 23),
            ],
            "TLB_TargetRead": [
              (Types("Intrinsic"), 24),
            ],
            "TLB_TargetWrite": [
              (Types("Intrinsic"), 25),
            ],
            "TLB_RWrite": [
              (Types("Intrinsic"), 26),
            ],
            "TLB_RWriteLock": [
              (Types("Intrinsic"), 27),
            ],
            "TLB_Unlock": [
              (Types("Intrinsic"), 28),
            ],
            "TLB_Probe": [
              (Types("Intrinsic"), 29),
            ],
            "TLB_Invalidate": [
              (Types("Intrinsic"), 30),
            ],
            "TLB_FlushAll": [
              (Types("Intrinsic"), 31),
            ],
            "Rt": [
              (Types("BitField"), 32),
            ],
            "Ra": [
              (Types("BitField"), 33),
            ],
            "Rb": [
              (Types("BitField"), 34),
            ],
          },
        ),
      ),
      default_region: None,
      register_maps: [],
      pool: [
        (0, MemoryRegion(MemoryRegion(
          id: Id("const"),
          kind: Rom,
          size: 0,
          word_size: 1,
          is_default: false,
        ))),
        (1, Intrinsic(Intrinsic(
          id: Id("inst_start"),
        ))),
        (2, Intrinsic(Intrinsic(
          id: Id("inst_next"),
        ))),
        (3, Intrinsic(Intrinsic(
          id: Id("zext"),
        ))),
        (4, Intrinsic(Intrinsic(
          id: Id("sext"),
        ))),
        (5, Intrinsic(Intrinsic(
          id: Id("carry"),
        ))),
        (6, Intrinsic(Intrinsic(
          id: Id("scarry"),
        ))),
        (7, Intrinsic(Intrinsic(
          id: Id("sborrow"),
        ))),
        (8, Intrinsic(Intrinsic(
          id: Id("int2float"),
        ))),
        (9, Intrinsic(Intrinsic(
          id: Id("float2float"),
        ))),
        (10, Intrinsic(Intrinsic(
          id: Id("floor"),
        ))),
        (11, Intrinsic(Intrinsic(
          id: Id("mfsr"),
        ))),
        (12, Intrinsic(Intrinsic(
          id: Id("mtsr"),
        ))),
        (13, Intrinsic(Intrinsic(
          id: Id("msync"),
        ))),
        (14, Intrinsic(Intrinsic(
          id: Id("isync"),
        ))),
        (15, Intrinsic(Intrinsic(
          id: Id("dpref"),
        ))),
        (16, Intrinsic(Intrinsic(
          id: Id("dsb"),
        ))),
        (17, Intrinsic(Intrinsic(
          id: Id("isb"),
        ))),
        (18, Intrinsic(Intrinsic(
          id: Id("break"),
        ))),
        (19, Intrinsic(Intrinsic(
          id: Id("syscall"),
        ))),
        (20, Intrinsic(Intrinsic(
          id: Id("trap"),
        ))),
        (21, Intrinsic(Intrinsic(
          id: Id("cctl"),
        ))),
        (22, Intrinsic(Intrinsic(
          id: Id("setgie"),
        ))),
        (23, Intrinsic(Intrinsic(
          id: Id("setend"),
        ))),
        (24, Intrinsic(Intrinsic(
          id: Id("TLB_TargetRead"),
        ))),
        (25, Intrinsic(Intrinsic(
          id: Id("TLB_TargetWrite"),
        ))),
        (26, Intrinsic(Intrinsic(
          id: Id("TLB_RWrite"),
        ))),
        (27, Intrinsic(Intrinsic(
          id: Id("TLB_RWriteLock"),
        ))),
        (28, Intrinsic(Intrinsic(
          id: Id("TLB_Unlock"),
        ))),
        (29, Intrinsic(Intrinsic(
          id: Id("TLB_Probe"),
        ))),
        (30, Intrinsic(Intrinsic(
          id: Id("TLB_Invalidate"),
        ))),
        (31, Intrinsic(Intrinsic(
          id: Id("TLB_FlushAll"),
        ))),
        (32, BitField(BitField(
          id: Id("Rt"),
          bit_width: 32,
          start_bit: 20,
          end_bit: 24,
          is_signed: false,
          is_hex: false,
        ))),
        (33, BitField(BitField(
          id: Id("Ra"),
          bit_width: 32,
          start_bit: 20,
          end_bit: 24,
          is_signed: false,
          is_hex: true,
        ))),
        (34, BitField(BitField(
          id: Id("Rb"),
          bit_width: 32,
          start_bit: 20,
          end_bit: 24,
          is_signed: true,
          is_hex: false,
        ))),
      ],
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
            "const": [
              (Types("MemoryRegion"), 0),
            ],
            "inst_start": [
              (Types("Intrinsic"), 1),
            ],
            "inst_next": [
              (Types("Intrinsic"), 2),
            ],
            "zext": [
              (Types("Intrinsic"), 3),
            ],
            "sext": [
              (Types("Intrinsic"), 4),
            ],
            "carry": [
              (Types("Intrinsic"), 5),
            ],
            "scarry": [
              (Types("Intrinsic"), 6),
            ],
            "sborrow": [
              (Types("Intrinsic"), 7),
            ],
            "int2float": [
              (Types("Intrinsic"), 8),
            ],
            "float2float": [
              (Types("Intrinsic"), 9),
            ],
            "floor": [
              (Types("Intrinsic"), 10),
            ],
            "mfsr": [
              (Types("Intrinsic"), 11),
            ],
            "mtsr": [
              (Types("Intrinsic"), 12),
            ],
            "msync": [
              (Types("Intrinsic"), 13),
            ],
            "isync": [
              (Types("Intrinsic"), 14),
            ],
            "dpref": [
              (Types("Intrinsic"), 15),
            ],
            "dsb": [
              (Types("Intrinsic"), 16),
            ],
            "isb": [
              (Types("Intrinsic"), 17),
            ],
            "break": [
              (Types("Intrinsic"), 18),
            ],
            "syscall": [
              (Types("Intrinsic"), 19),
            ],
            "trap": [
              (Types("Intrinsic"), 20),
            ],
            "cctl": [
              (Types("Intrinsic"), 21),
            ],
            "setgie": [
              (Types("Intrinsic"), 22),
            ],
            "setend": [
              (Types("Intrinsic"), 23),
            ],
            "TLB_TargetRead": [
              (Types("Intrinsic"), 24),
            ],
            "TLB_TargetWrite": [
              (Types("Intrinsic"), 25),
            ],
            "TLB_RWrite": [
              (Types("Intrinsic"), 26),
            ],
            "TLB_RWriteLock": [
              (Types("Intrinsic"), 27),
            ],
            "TLB_Unlock": [
              (Types("Intrinsic"), 28),
            ],
            "TLB_Probe": [
              (Types("Intrinsic"), 29),
            ],
            "TLB_Invalidate": [
              (Types("Intrinsic"), 30),
            ],
            "TLB_FlushAll": [
              (Types("Intrinsic"), 31),
            ],
            "Rt": [
              (Types("BitField"), 32),
            ],
            "Ra": [
              (Types("BitField"), 33),
            ],
            "foo": [
              (Types("Scanner"), 34),
            ],
          },
        ),
      ),
      default_region: None,
      register_maps: [],
      pool: [
        (0, MemoryRegion(MemoryRegion(
          id: Id("const"),
          kind: Rom,
          size: 0,
          word_size: 1,
          is_default: false,
        ))),
        (1, Intrinsic(Intrinsic(
          id: Id("inst_start"),
        ))),
        (2, Intrinsic(Intrinsic(
          id: Id("inst_next"),
        ))),
        (3, Intrinsic(Intrinsic(
          id: Id("zext"),
        ))),
        (4, Intrinsic(Intrinsic(
          id: Id("sext"),
        ))),
        (5, Intrinsic(Intrinsic(
          id: Id("carry"),
        ))),
        (6, Intrinsic(Intrinsic(
          id: Id("scarry"),
        ))),
        (7, Intrinsic(Intrinsic(
          id: Id("sborrow"),
        ))),
        (8, Intrinsic(Intrinsic(
          id: Id("int2float"),
        ))),
        (9, Intrinsic(Intrinsic(
          id: Id("float2float"),
        ))),
        (10, Intrinsic(Intrinsic(
          id: Id("floor"),
        ))),
        (11, Intrinsic(Intrinsic(
          id: Id("mfsr"),
        ))),
        (12, Intrinsic(Intrinsic(
          id: Id("mtsr"),
        ))),
        (13, Intrinsic(Intrinsic(
          id: Id("msync"),
        ))),
        (14, Intrinsic(Intrinsic(
          id: Id("isync"),
        ))),
        (15, Intrinsic(Intrinsic(
          id: Id("dpref"),
        ))),
        (16, Intrinsic(Intrinsic(
          id: Id("dsb"),
        ))),
        (17, Intrinsic(Intrinsic(
          id: Id("isb"),
        ))),
        (18, Intrinsic(Intrinsic(
          id: Id("break"),
        ))),
        (19, Intrinsic(Intrinsic(
          id: Id("syscall"),
        ))),
        (20, Intrinsic(Intrinsic(
          id: Id("trap"),
        ))),
        (21, Intrinsic(Intrinsic(
          id: Id("cctl"),
        ))),
        (22, Intrinsic(Intrinsic(
          id: Id("setgie"),
        ))),
        (23, Intrinsic(Intrinsic(
          id: Id("setend"),
        ))),
        (24, Intrinsic(Intrinsic(
          id: Id("TLB_TargetRead"),
        ))),
        (25, Intrinsic(Intrinsic(
          id: Id("TLB_TargetWrite"),
        ))),
        (26, Intrinsic(Intrinsic(
          id: Id("TLB_RWrite"),
        ))),
        (27, Intrinsic(Intrinsic(
          id: Id("TLB_RWriteLock"),
        ))),
        (28, Intrinsic(Intrinsic(
          id: Id("TLB_Unlock"),
        ))),
        (29, Intrinsic(Intrinsic(
          id: Id("TLB_Probe"),
        ))),
        (30, Intrinsic(Intrinsic(
          id: Id("TLB_Invalidate"),
        ))),
        (31, Intrinsic(Intrinsic(
          id: Id("TLB_FlushAll"),
        ))),
        (32, BitField(BitField(
          id: Id("Rt"),
          bit_width: 32,
          start_bit: 20,
          end_bit: 24,
          is_signed: false,
          is_hex: false,
        ))),
        (33, BitField(BitField(
          id: Id("Ra"),
          bit_width: 32,
          start_bit: 10,
          end_bit: 16,
          is_signed: false,
          is_hex: false,
        ))),
        (34, Scanner(Scanner(
          id: Id("foo"),
          rules: [
            35,
            36,
          ],
          is_instruction: false,
        ))),
        (35, Rule(Rule(
          id: Id("foo"),
          mnemonic: [],
          output: [
            Expr(32),
          ],
          setup: [],
          actions: [],
          pattern: [],
          scope: Scope(
            env: TypeEnv(
              env: {},
            ),
          ),
        ))),
        (36, Rule(Rule(
          id: Id("foo"),
          mnemonic: [],
          output: [
            Expr(33),
          ],
          setup: [],
          actions: [],
          pattern: [],
          scope: Scope(
            env: TypeEnv(
              env: {},
            ),
          ),
        ))),
      ],
    )
    "#);
    Ok(())
}
