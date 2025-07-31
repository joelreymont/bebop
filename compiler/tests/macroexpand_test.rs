use bebop_compiler::{env::*, error::*, ir::*};
use bebop_util::id::*;
use insta::*;

mod common;

#[test]
fn test_expand() -> Result<(), Error> {
    let s = r"
    define token instr32(32)
        Rt = (20, 24)
        Ra = (20, 24)
        Rb = (20, 24)
    ;
    macro add(dst, a, b)
    {
        local dst_ = a + b;
        dst = dst_;
    }
    macro sub(dst, a, b)
    {
        local dst_ = a - b;
        dst = dst_;
    }
    :test Rt, Ra, Rb is
    {
      add(Rt, Ra, Rb);
      sub(Rt, Ra, Rb);
    }
    ";
    let id = MetaId::from("test");
    let mut arch = common::parse_and_lift(s)?;
    let expr = arch.env.lookup(&id, Kind::Scanner)?;
    expr.apply(|x| {
        let scanner: &Scanner = x.try_into()?;
        scanner.rules[0].apply(|x| {
            let rule: &Rule = x.try_into()?;
            assert_ron_snapshot!(rule.env, @r"
            Env(
              env: TypeMap(
                inner: {},
              ),
            )
            ");
            assert_ron_snapshot!(rule.actions, @r#"
            [
              MacroCall(MacroCall(
                macro: Macro(Macro(
                  id: MetaId("add"),
                  args: [
                    MetaId("dst"),
                    MetaId("a"),
                    MetaId("b"),
                  ],
                  actions: [
                    Bind(
                      lhs: Variable(Variable(
                        id: MetaId("dst_"),
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
                        id: MetaId("dst_"),
                      )),
                    ),
                  ],
                  env: Env(
                    env: TypeMap(
                      inner: {
                        Key(
                          id: "dst",
                          kind: Kind("Variable"),
                        ): Value(
                          expr: Variable(Variable(
                            id: MetaId("dst"),
                          )),
                        ),
                        Key(
                          id: "a",
                          kind: Kind("Variable"),
                        ): Value(
                          expr: Variable(Variable(
                            id: MetaId("a"),
                          )),
                        ),
                        Key(
                          id: "b",
                          kind: Kind("Variable"),
                        ): Value(
                          expr: Variable(Variable(
                            id: MetaId("b"),
                          )),
                        ),
                        Key(
                          id: "dst_",
                          kind: Kind("Variable"),
                        ): Value(
                          expr: Variable(Variable(
                            id: MetaId("dst_"),
                          )),
                        ),
                      },
                    ),
                  ),
                )),
                args: [
                  BitField(BitField(
                    id: MetaId("Rt"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                  BitField(BitField(
                    id: MetaId("Ra"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                  BitField(BitField(
                    id: MetaId("Rb"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                ],
              )),
              MacroCall(MacroCall(
                macro: Macro(Macro(
                  id: MetaId("sub"),
                  args: [
                    MetaId("dst"),
                    MetaId("a"),
                    MetaId("b"),
                  ],
                  actions: [
                    Bind(
                      lhs: Variable(Variable(
                        id: MetaId("dst_"),
                      )),
                      rhs: Binary(
                        op: MINUS(Unsigned),
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
                        id: MetaId("dst_"),
                      )),
                    ),
                  ],
                  env: Env(
                    env: TypeMap(
                      inner: {
                        Key(
                          id: "dst",
                          kind: Kind("Variable"),
                        ): Value(
                          expr: Variable(Variable(
                            id: MetaId("dst"),
                          )),
                        ),
                        Key(
                          id: "a",
                          kind: Kind("Variable"),
                        ): Value(
                          expr: Variable(Variable(
                            id: MetaId("a"),
                          )),
                        ),
                        Key(
                          id: "b",
                          kind: Kind("Variable"),
                        ): Value(
                          expr: Variable(Variable(
                            id: MetaId("b"),
                          )),
                        ),
                        Key(
                          id: "dst_",
                          kind: Kind("Variable"),
                        ): Value(
                          expr: Variable(Variable(
                            id: MetaId("dst_"),
                          )),
                        ),
                      },
                    ),
                  ),
                )),
                args: [
                  BitField(BitField(
                    id: MetaId("Rt"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                  BitField(BitField(
                    id: MetaId("Ra"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                  BitField(BitField(
                    id: MetaId("Rb"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                ],
              )),
            ]
            "#);
            Ok(())
        })
    })?;
    reset_unique_id_counter();
    arch.macroexpand()?;
    expr.apply(|x| {
        let scanner: &Scanner = x.try_into()?;
        scanner.rules[0].apply(|x| {
            let rule: &Rule = x.try_into()?;
            assert_ron_snapshot!(rule.env, @r#"
            Env(
              env: TypeMap(
                inner: {
                  Key(
                    id: "dst_0",
                    kind: Kind("Variable"),
                  ): Value(
                    expr: Variable(Variable(
                      id: MetaId("dst_0"),
                    )),
                  ),
                  Key(
                    id: "dst_1",
                    kind: Kind("Variable"),
                  ): Value(
                    expr: Variable(Variable(
                      id: MetaId("dst_1"),
                    )),
                  ),
                },
              ),
            )
            "#);
            assert_ron_snapshot!(rule.actions, @r#"
            [
              Bind(
                lhs: Variable(Variable(
                  id: MetaId("dst_0"),
                )),
                rhs: Binary(
                  op: PLUS(Unsigned),
                  lhs: BitField(BitField(
                    id: MetaId("Ra"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                  rhs: BitField(BitField(
                    id: MetaId("Rb"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                ),
              ),
              Bind(
                lhs: BitField(BitField(
                  id: MetaId("Rt"),
                  bit_width: 32,
                  start_bit: 20,
                  end_bit: 24,
                  is_signed: false,
                  is_hex: false,
                )),
                rhs: Variable(Variable(
                  id: MetaId("dst_0"),
                )),
              ),
              Bind(
                lhs: Variable(Variable(
                  id: MetaId("dst_1"),
                )),
                rhs: Binary(
                  op: MINUS(Unsigned),
                  lhs: BitField(BitField(
                    id: MetaId("Ra"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                  rhs: BitField(BitField(
                    id: MetaId("Rb"),
                    bit_width: 32,
                    start_bit: 20,
                    end_bit: 24,
                    is_signed: false,
                    is_hex: false,
                  )),
                ),
              ),
              Bind(
                lhs: BitField(BitField(
                  id: MetaId("Rt"),
                  bit_width: 32,
                  start_bit: 20,
                  end_bit: 24,
                  is_signed: false,
                  is_hex: false,
                )),
                rhs: Variable(Variable(
                  id: MetaId("dst_1"),
                )),
              ),
            ]
            "#);
            Ok(())
        })
    })?;
    Ok(())
}
