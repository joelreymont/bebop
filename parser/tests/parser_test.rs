use bebop_parser::{error::*, parse, *};
use insta::*;

#[test]
fn parse_def_endian() -> Result<(), ParserError> {
    let s = r"
      define endian = big;
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r"
    [
      Endian(Big),
    ]
    ");
    Ok(())
}

#[test]
fn parse_def_alignment() -> Result<(), ParserError> {
    let s = r"
      define alignment = 0x200;
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r"
    [
      Alignment(512),
    ]
    ");
    Ok(())
}

#[test]
fn parse_def_space1() -> Result<(), ParserError> {
    let s = r"
      define space ram type=ram_space size=4 wordsize=1 default;
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Space(Space(
        id: "ram",
        kind: Ram,
        size: 4,
        word_size: 1,
        is_default: true,
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn parse_def_space2() -> Result<(), ParserError> {
    let s = r"
      define space register type=register_space size=4;
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Space(Space(
        id: "register",
        kind: Register,
        size: 4,
        word_size: 1,
        is_default: true,
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn parse_def_register() -> Result<(), ParserError> {
    let s = r"
      define register offset=0x100 size=4
      [r0 r1 r2 r3];
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Varnode(Varnode(
        offset: 256,
        byte_size: 4,
        ids: [
          "r0",
          "r1",
          "r2",
          "r3",
        ],
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn parse_def_token() -> Result<(), ParserError> {
    let s = r"
      define token instr32(32)
          OpSz        = (31, 31)
          Opc         = (25, 30) signed
          Rt          = (20, 24) hex
      ;
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Token(Token(
        id: "instr32",
        bit_width: 32,
        fields: [
          Field(
            id: "OpSz",
            start_bit: 31,
            end_bit: 31,
            is_signed: false,
            is_hex: false,
          ),
          Field(
            id: "Opc",
            start_bit: 25,
            end_bit: 30,
            is_signed: true,
            is_hex: false,
          ),
          Field(
            id: "Rt",
            start_bit: 20,
            end_bit: 24,
            is_signed: false,
            is_hex: true,
          ),
        ],
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn parse_def_varnode_attach() -> Result<(), ParserError> {
    let s = r"
      attach variables [Rt Rs] [
          r0 _ r1
      ];
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      VarnodeAttach(VarnodeAttach(
        fields: [
          "Rt",
          "Rs",
        ],
        registers: [
          "r0",
          "_",
          "r1",
        ],
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn parse_ctr_start1() -> Result<(), ParserError> {
    let s = r"foo: Rt is";
    let ast = parse(CtrStartParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    ("foo", Display(
      mnemonic: [],
      output: [
        Id("Rt"),
        Space,
      ],
    ), false)
    "#);
    Ok(())
}

#[test]
fn parse_ctr_start2() -> Result<(), ParserError> {
    let s = r":foo^bar Rt is";
    let ast = parse(CtrStartParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    ("foo", Display(
      mnemonic: [
        Caret,
        Id("bar"),
      ],
      output: [
        Id("Rt"),
        Space,
      ],
    ), true)
    "#);
    Ok(())
}

#[test]
fn parse_ctr_no_mnemonic() -> Result<(), ParserError> {
    let s = r"foo: Rt is unimpl";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Constructor(Constructor(
        id: "foo",
        display: Display(
          mnemonic: [],
          output: [
            Id("Rt"),
            Space,
          ],
        ),
        pattern: None,
        context: [],
        body: [],
        is_instruction: false,
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn parse_constructor1() -> Result<(), ParserError> {
    let s = r"
        :bse is OpSz=0 & Opc=0b100001 & Rt & Ra & Rb & Alu2Mod=0b0000 & Sub6=0b001100 unimpl
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Constructor(Constructor(
        id: "bse",
        display: Display(
          mnemonic: [],
          output: [],
        ),
        pattern: Some(Binary(
          op: AND,
          lhs: Binary(
            op: AND,
            lhs: Binary(
              op: AND,
              lhs: Binary(
                op: AND,
                lhs: Binary(
                  op: AND,
                  lhs: Binary(
                    op: AND,
                    lhs: Binary(
                      op: EQ,
                      lhs: Id("OpSz"),
                      rhs: Int(0),
                    ),
                    rhs: Binary(
                      op: EQ,
                      lhs: Id("Opc"),
                      rhs: Int(33),
                    ),
                  ),
                  rhs: Id("Rt"),
                ),
                rhs: Id("Ra"),
              ),
              rhs: Id("Rb"),
            ),
            rhs: Binary(
              op: EQ,
              lhs: Id("Alu2Mod"),
              rhs: Int(0),
            ),
          ),
          rhs: Binary(
            op: EQ,
            lhs: Id("Sub6"),
            rhs: Int(12),
          ),
        )),
        context: [],
        body: [],
        is_instruction: true,
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn parse_constructor2() -> Result<(), ParserError> {
    let s = r"
        :fmtsr FRt, FSa is FOpSz=0 & COP=0b110101 & FRt & FSa & MxCP=0b1001 { FSa = FRt; }
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Constructor(Constructor(
        id: "fmtsr",
        display: Display(
          mnemonic: [],
          output: [
            Id("FRt"),
            Text(","),
            Space,
            Id("FSa"),
            Space,
          ],
        ),
        pattern: Some(Binary(
          op: AND,
          lhs: Binary(
            op: AND,
            lhs: Binary(
              op: AND,
              lhs: Binary(
                op: AND,
                lhs: Binary(
                  op: EQ,
                  lhs: Id("FOpSz"),
                  rhs: Int(0),
                ),
                rhs: Binary(
                  op: EQ,
                  lhs: Id("COP"),
                  rhs: Int(53),
                ),
              ),
              rhs: Id("FRt"),
            ),
            rhs: Id("FSa"),
          ),
          rhs: Binary(
            op: EQ,
            lhs: Id("MxCP"),
            rhs: Int(9),
          ),
        )),
        context: [],
        body: [
          Bind(
            lhs: Id("FSa"),
            rhs: Id("FRt"),
          ),
        ],
        is_instruction: true,
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn parse_constructor3() -> Result<(), ParserError> {
    let s = r"
        :ADC OP1     is (cc=1 & aaa=3) ... & OP1 unimpl
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Constructor(Constructor(
        id: "ADC",
        display: Display(
          mnemonic: [],
          output: [
            Id("OP1"),
            Space,
          ],
        ),
        pattern: Some(Binary(
          op: AND,
          lhs: Unary(
            op: AlignLeft,
            rhs: Paren(Binary(
              op: AND,
              lhs: Binary(
                op: EQ,
                lhs: Id("cc"),
                rhs: Int(1),
              ),
              rhs: Binary(
                op: EQ,
                lhs: Id("aaa"),
                rhs: Int(3),
              ),
            )),
          ),
          rhs: Id("OP1"),
        )),
        context: [],
        body: [],
        is_instruction: true,
      )),
    ]
    "#);
    Ok(())
}
