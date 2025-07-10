use bebop_sleigh_parser::{error::*, parse, *};
use insta::*;

#[test]
fn test_def_endian() -> Result<(), ParseError> {
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
fn test_def_alignment() -> Result<(), ParseError> {
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
fn test_def_space1() -> Result<(), ParseError> {
    let s = r"
      define space ram type=ram_space size=4 wordsize=1 default;
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Space(Space(
        id: Ident("ram"),
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
fn test_def_space2() -> Result<(), ParseError> {
    let s = r"
      define space register type=register_space size=4;
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Space(Space(
        id: Ident("register"),
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
fn test_def_register() -> Result<(), ParseError> {
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
          Ident("r0"),
          Ident("r1"),
          Ident("r2"),
          Ident("r3"),
        ],
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn test_def_token() -> Result<(), ParseError> {
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
        id: Ident("instr32"),
        bit_width: 32,
        fields: [
          Field(
            id: Ident("OpSz"),
            start_bit: 31,
            end_bit: 31,
            is_signed: false,
            is_hex: false,
          ),
          Field(
            id: Ident("Opc"),
            start_bit: 25,
            end_bit: 30,
            is_signed: true,
            is_hex: false,
          ),
          Field(
            id: Ident("Rt"),
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
fn test_def_varnode_attach() -> Result<(), ParseError> {
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
          Ident("Rt"),
          Ident("Rs"),
        ],
        registers: [
          Ident("r0"),
          Ident("_"),
          Ident("r1"),
        ],
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn test_ctr_start1() -> Result<(), ParseError> {
    let s = r"foo: Rt is";
    let ast = parse(CtrStartParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    (Ident("foo"), Display(
      mnemonic: [],
      output: [
        Id(Ident("Rt")),
        Space,
      ],
    ), false)
    "#);
    Ok(())
}

#[test]
fn test_ctr_start2() -> Result<(), ParseError> {
    let s = r":foo^bar Rt is";
    let ast = parse(CtrStartParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    (Ident("foo"), Display(
      mnemonic: [
        Caret,
        Id(Ident("bar")),
      ],
      output: [
        Id(Ident("Rt")),
        Space,
      ],
    ), true)
    "#);
    Ok(())
}

#[test]
fn test_ctr_no_mnemonic() -> Result<(), ParseError> {
    let s = r"foo: Rt is unimpl";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Constructor(Constructor(
        id: Ident("foo"),
        display: Display(
          mnemonic: [],
          output: [
            Id(Ident("Rt")),
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
fn test_constructor1() -> Result<(), ParseError> {
    let s = r"
        :bse is OpSz=0 & Opc=0b100001 & Rt & Ra & Rb & Alu2Mod=0b0000 & Sub6=0b001100 unimpl
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Constructor(Constructor(
        id: Ident("bse"),
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
                      lhs: Id(Ident("OpSz")),
                      rhs: Int(0),
                    ),
                    rhs: Binary(
                      op: EQ,
                      lhs: Id(Ident("Opc")),
                      rhs: Int(33),
                    ),
                  ),
                  rhs: Id(Ident("Rt")),
                ),
                rhs: Id(Ident("Ra")),
              ),
              rhs: Id(Ident("Rb")),
            ),
            rhs: Binary(
              op: EQ,
              lhs: Id(Ident("Alu2Mod")),
              rhs: Int(0),
            ),
          ),
          rhs: Binary(
            op: EQ,
            lhs: Id(Ident("Sub6")),
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
fn test_constructor2() -> Result<(), ParseError> {
    let s = r"
        :fmtsr FRt, FSa is FOpSz=0 & COP=0b110101 & FRt & FSa & MxCP=0b1001 { FSa = FRt; }
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Constructor(Constructor(
        id: Ident("fmtsr"),
        display: Display(
          mnemonic: [],
          output: [
            Id(Ident("FRt")),
            Text(Ident(",")),
            Space,
            Id(Ident("FSa")),
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
                  lhs: Id(Ident("FOpSz")),
                  rhs: Int(0),
                ),
                rhs: Binary(
                  op: EQ,
                  lhs: Id(Ident("COP")),
                  rhs: Int(53),
                ),
              ),
              rhs: Id(Ident("FRt")),
            ),
            rhs: Id(Ident("FSa")),
          ),
          rhs: Binary(
            op: EQ,
            lhs: Id(Ident("MxCP")),
            rhs: Int(9),
          ),
        )),
        context: [],
        body: [
          Bind(
            lhs: Id(Ident("FSa")),
            rhs: Id(Ident("FRt")),
          ),
        ],
        is_instruction: true,
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn test_constructor3() -> Result<(), ParseError> {
    let s = r"
        :ADC OP1     is (cc=1 & aaa=3) ... & OP1 unimpl
    ";
    let ast = parse(DefsParserEx::new(), s)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Constructor(Constructor(
        id: Ident("ADC"),
        display: Display(
          mnemonic: [],
          output: [
            Id(Ident("OP1")),
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
                lhs: Id(Ident("cc")),
                rhs: Int(1),
              ),
              rhs: Binary(
                op: EQ,
                lhs: Id(Ident("aaa")),
                rhs: Int(3),
              ),
            )),
          ),
          rhs: Id(Ident("OP1")),
        )),
        context: [],
        body: [],
        is_instruction: true,
      )),
    ]
    "#);
    Ok(())
}
