use bebop_sleigh_compiler::{error::*, hir::*};
use bebop_sleigh_parser::{parse, *};
use insta::*;

fn parse_and_lift(s: &str) -> Result<Architecture, LiftError> {
    let ast = parse(DefsParserEx::new(), s).map_err(LiftError::ParserError)?;
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
        env: [
          ("Ra", BitField(BitField(
            id: "Ra",
            bit_width: 32,
            start_bit: 20,
            end_bit: 24,
            is_signed: false,
            is_hex: true,
          ))),
          ("Rb", BitField(BitField(
            id: "Rb",
            bit_width: 32,
            start_bit: 20,
            end_bit: 24,
            is_signed: true,
            is_hex: false,
          ))),
          ("Rt", BitField(BitField(
            id: "Rt",
            bit_width: 32,
            start_bit: 20,
            end_bit: 24,
            is_signed: false,
            is_hex: false,
          ))),
        ],
      ),
      default_region: None,
      scanners: [],
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
        ;
        foo: Rt is unimpl
    ";
    let arch = parse_and_lift(s)?;
    assert_ron_snapshot!(arch.scanners, @r#"
    [
      Scanner(
        id: "foo",
        rules: [
          Rule(
            id: "foo",
            mnemonic: [],
            output: [
              BitField(BitField(
                id: "Rt",
                bit_width: 32,
                start_bit: 20,
                end_bit: 24,
                is_signed: false,
                is_hex: false,
              )),
            ],
            setup: [],
            actions: [],
            pattern: [],
            scope: Scope(
              env: [],
            ),
          ),
        ],
        is_instruction: false,
      ),
    ]
    "#);
    Ok(())
}
