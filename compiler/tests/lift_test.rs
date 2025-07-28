use bebop_compiler::{env::*, error::*, ir::*};
use bebop_util::id::*;
use insta::*;

mod common;

#[test]
fn test_lift_token() -> Result<(), Error> {
    let s = r"
        define token instr32(32)
            Rt          = (20, 24)
            Ra          = (20, 24) hex
            Rb          = (20, 24) signed
        ;
    ";
    let arch = common::parse_and_lift(s)?;
    assert_ron_snapshot!(arch, @r#"
    Architecture(
      endian: Endian(Little),
      alignment: Alignment(4),
      env: Env(
        env: TypeMap(
          inner: {
            Key(
              id: "const",
              kind: Kind("MemoryRegion"),
            ): Value(
              expr: MemoryRegion(MemoryRegion(
                id: MetaId("const"),
                kind: Rom,
                size: 0,
                word_size: 1,
                is_default: false,
              )),
            ),
            Key(
              id: "inst_start",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("inst_start"),
              )),
            ),
            Key(
              id: "inst_next",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("inst_next"),
              )),
            ),
            Key(
              id: "zext",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("zext"),
              )),
            ),
            Key(
              id: "sext",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("sext"),
              )),
            ),
            Key(
              id: "carry",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("carry"),
              )),
            ),
            Key(
              id: "scarry",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("scarry"),
              )),
            ),
            Key(
              id: "sborrow",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("sborrow"),
              )),
            ),
            Key(
              id: "int2float",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("int2float"),
              )),
            ),
            Key(
              id: "float2float",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("float2float"),
              )),
            ),
            Key(
              id: "floor",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("floor"),
              )),
            ),
            Key(
              id: "Rt",
              kind: Kind("BitField"),
            ): Value(
              expr: BitField(BitField(
                id: MetaId("Rt"),
                bit_width: 32,
                start_bit: 20,
                end_bit: 24,
                is_signed: false,
                is_hex: false,
              )),
            ),
            Key(
              id: "Ra",
              kind: Kind("BitField"),
            ): Value(
              expr: BitField(BitField(
                id: MetaId("Ra"),
                bit_width: 32,
                start_bit: 20,
                end_bit: 24,
                is_signed: false,
                is_hex: true,
              )),
            ),
            Key(
              id: "Rb",
              kind: Kind("BitField"),
            ): Value(
              expr: BitField(BitField(
                id: MetaId("Rb"),
                bit_width: 32,
                start_bit: 20,
                end_bit: 24,
                is_signed: true,
                is_hex: false,
              )),
            ),
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
fn test_lift_ctr_simple() -> Result<(), Error> {
    let s = r"
        define token instr32(32)
            Rt          = (20, 24)
            Ra          = (10, 16)
        ;
        foo: Rt is unimpl
        foo: Ra is unimpl
    ";
    let arch = common::parse_and_lift(s)?;
    assert_ron_snapshot!(arch, @r#"
    Architecture(
      endian: Endian(Little),
      alignment: Alignment(4),
      env: Env(
        env: TypeMap(
          inner: {
            Key(
              id: "const",
              kind: Kind("MemoryRegion"),
            ): Value(
              expr: MemoryRegion(MemoryRegion(
                id: MetaId("const"),
                kind: Rom,
                size: 0,
                word_size: 1,
                is_default: false,
              )),
            ),
            Key(
              id: "inst_start",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("inst_start"),
              )),
            ),
            Key(
              id: "inst_next",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("inst_next"),
              )),
            ),
            Key(
              id: "zext",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("zext"),
              )),
            ),
            Key(
              id: "sext",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("sext"),
              )),
            ),
            Key(
              id: "carry",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("carry"),
              )),
            ),
            Key(
              id: "scarry",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("scarry"),
              )),
            ),
            Key(
              id: "sborrow",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("sborrow"),
              )),
            ),
            Key(
              id: "int2float",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("int2float"),
              )),
            ),
            Key(
              id: "float2float",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("float2float"),
              )),
            ),
            Key(
              id: "floor",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("floor"),
              )),
            ),
            Key(
              id: "Rt",
              kind: Kind("BitField"),
            ): Value(
              expr: BitField(BitField(
                id: MetaId("Rt"),
                bit_width: 32,
                start_bit: 20,
                end_bit: 24,
                is_signed: false,
                is_hex: false,
              )),
            ),
            Key(
              id: "Ra",
              kind: Kind("BitField"),
            ): Value(
              expr: BitField(BitField(
                id: MetaId("Ra"),
                bit_width: 32,
                start_bit: 10,
                end_bit: 16,
                is_signed: false,
                is_hex: false,
              )),
            ),
            Key(
              id: "foo",
              kind: Kind("Scanner"),
            ): Value(
              expr: Scanner(Scanner(
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
                    env: Env(
                      env: TypeMap(
                        inner: {},
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
                    env: Env(
                      env: TypeMap(
                        inner: {},
                      ),
                    ),
                  )),
                ],
                is_instruction: false,
              )),
            ),
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
fn test_lift_macro() -> Result<(), Error> {
    let s = r"
    macro add(dst, a, b)
    {
        local dst_ = a + b;
        dst = dst_;
    }
    ";
    let arch = common::parse_and_lift(s)?;
    let mut expr = arch.env.lookup(&MetaId::from("add"), Kind::Macro)?;
    expr.apply_mut(|x| {
        let r#macro: &mut Macro = x.try_into()?;
        r#macro.env.make_unique()?;
        Ok(())
    })?;
    assert_ron_snapshot!(arch, @r#"
    Architecture(
      endian: Endian(Little),
      alignment: Alignment(4),
      env: Env(
        env: TypeMap(
          inner: {
            Key(
              id: "const",
              kind: Kind("MemoryRegion"),
            ): Value(
              expr: MemoryRegion(MemoryRegion(
                id: MetaId("const"),
                kind: Rom,
                size: 0,
                word_size: 1,
                is_default: false,
              )),
            ),
            Key(
              id: "inst_start",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("inst_start"),
              )),
            ),
            Key(
              id: "inst_next",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("inst_next"),
              )),
            ),
            Key(
              id: "zext",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("zext"),
              )),
            ),
            Key(
              id: "sext",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("sext"),
              )),
            ),
            Key(
              id: "carry",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("carry"),
              )),
            ),
            Key(
              id: "scarry",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("scarry"),
              )),
            ),
            Key(
              id: "sborrow",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("sborrow"),
              )),
            ),
            Key(
              id: "int2float",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("int2float"),
              )),
            ),
            Key(
              id: "float2float",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("float2float"),
              )),
            ),
            Key(
              id: "floor",
              kind: Kind("Intrinsic"),
            ): Value(
              expr: Intrinsic(Intrinsic(
                id: MetaId("floor"),
              )),
            ),
            Key(
              id: "add",
              kind: Kind("Macro"),
            ): Value(
              expr: Macro(Macro(
                id: MetaId("add"),
                args: [
                  MetaId("dst"),
                  MetaId("a"),
                  MetaId("b"),
                ],
                body: [
                  Bind(
                    lhs: Variable(Variable(
                      id: MetaId("dst_3"),
                    )),
                    rhs: Binary(
                      op: PLUS(Unsigned),
                      lhs: Variable(Variable(
                        id: MetaId("a1"),
                      )),
                      rhs: Variable(Variable(
                        id: MetaId("b2"),
                      )),
                    ),
                  ),
                  Bind(
                    lhs: Variable(Variable(
                      id: MetaId("dst0"),
                    )),
                    rhs: Variable(Variable(
                      id: MetaId("dst_3"),
                    )),
                  ),
                ],
                env: Env(
                  env: TypeMap(
                    inner: {
                      Key(
                        id: "dst0",
                        kind: Kind("Variable"),
                      ): Value(
                        expr: Variable(Variable(
                          id: MetaId("dst0"),
                        )),
                      ),
                      Key(
                        id: "a1",
                        kind: Kind("Variable"),
                      ): Value(
                        expr: Variable(Variable(
                          id: MetaId("a1"),
                        )),
                      ),
                      Key(
                        id: "b2",
                        kind: Kind("Variable"),
                      ): Value(
                        expr: Variable(Variable(
                          id: MetaId("b2"),
                        )),
                      ),
                      Key(
                        id: "dst_3",
                        kind: Kind("Variable"),
                      ): Value(
                        expr: Variable(Variable(
                          id: MetaId("dst_3"),
                        )),
                      ),
                    },
                  ),
                ),
              )),
            ),
          },
        ),
      ),
      default_region: None,
      register_maps: [],
    )
    "#);
    Ok(())
}
