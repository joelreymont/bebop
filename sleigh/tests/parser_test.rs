use bebop_sleigh::{ast::Context, error::*, grammar, lexer::*, meta::*};
use insta::*;

#[test]
fn test_def_endian() -> Result<(), ParseError> {
    let s = r"
      define endian = big;
    ";
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let mut context = Context::new();
    let ast = parser.parse(FileId::empty(), &mut context, lexer)?;
    assert_ron_snapshot!(ast, @r"
    [
      Endian(Tagged(
        value: Big,
        tag: Span(
          src: FileId([]),
          range: (7, 27),
        ),
      )),
    ]
    ");
    Ok(())
}

#[test]
fn test_def_alignment() -> Result<(), ParseError> {
    let s = r"
      define alignment = 0x200;
    ";
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let mut context = Context::new();
    let ast = parser.parse(FileId::empty(), &mut context, lexer)?;
    assert_ron_snapshot!(ast, @r"
    [
      Alignment(Tagged(
        value: 512,
        tag: Span(
          src: FileId([]),
          range: (7, 32),
        ),
      )),
    ]
    ");
    Ok(())
}

#[test]
fn test_def_space1() -> Result<(), ParseError> {
    let s = r"
      define space ram type=ram_space size=4 wordsize=1 default;
    ";
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let mut context = Context::new();
    let ast = parser.parse(FileId::empty(), &mut context, lexer)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Space(Tagged(
        value: Space(
          id: Tagged(
            value: Ident("ram"),
            tag: Span(
              src: FileId([]),
              range: (20, 23),
            ),
          ),
          kind: Ram,
          size: 4,
          word_size: 1,
          is_default: true,
        ),
        tag: Span(
          src: FileId([]),
          range: (7, 65),
        ),
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
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let mut context = Context::new();
    let ast = parser.parse(FileId::empty(), &mut context, lexer)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Space(Tagged(
        value: Space(
          id: Tagged(
            value: Ident("register"),
            tag: Span(
              src: FileId([]),
              range: (20, 28),
            ),
          ),
          kind: Register,
          size: 4,
          word_size: 1,
          is_default: true,
        ),
        tag: Span(
          src: FileId([]),
          range: (7, 56),
        ),
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
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let mut context = Context::new();
    let ast = parser.parse(FileId::empty(), &mut context, lexer)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Varnode(Tagged(
        value: Varnode(
          offset: 256,
          size: 4,
          ids: [
            Tagged(
              value: Ident("r0"),
              tag: Span(
                src: FileId([]),
                range: (50, 52),
              ),
            ),
            Tagged(
              value: Ident("r1"),
              tag: Span(
                src: FileId([]),
                range: (53, 55),
              ),
            ),
            Tagged(
              value: Ident("r2"),
              tag: Span(
                src: FileId([]),
                range: (56, 58),
              ),
            ),
            Tagged(
              value: Ident("r3"),
              tag: Span(
                src: FileId([]),
                range: (59, 61),
              ),
            ),
          ],
        ),
        tag: Span(
          src: FileId([]),
          range: (7, 63),
        ),
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
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let mut context = Context::new();
    let ast = parser.parse(FileId::empty(), &mut context, lexer)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Token(Tagged(
        value: Token(
          id: Tagged(
            value: Ident("instr32"),
            tag: Span(
              src: FileId([]),
              range: (20, 27),
            ),
          ),
          bit_width: 32,
          fields: [
            Field(
              id: Tagged(
                value: Ident("OpSz"),
                tag: Span(
                  src: FileId([]),
                  range: (42, 46),
                ),
              ),
              start_bit: 31,
              end_bit: 31,
              is_signed: false,
              is_hex: false,
            ),
            Field(
              id: Tagged(
                value: Ident("Opc"),
                tag: Span(
                  src: FileId([]),
                  range: (75, 78),
                ),
              ),
              start_bit: 25,
              end_bit: 30,
              is_signed: true,
              is_hex: false,
            ),
            Field(
              id: Tagged(
                value: Ident("Rt"),
                tag: Span(
                  src: FileId([]),
                  range: (115, 117),
                ),
              ),
              start_bit: 20,
              end_bit: 24,
              is_signed: false,
              is_hex: true,
            ),
          ],
        ),
        tag: Span(
          src: FileId([]),
          range: (7, 149),
        ),
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
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let mut context = Context::new();
    let ast = parser.parse(FileId::empty(), &mut context, lexer)?;
    assert_ron_snapshot!(ast, @r#"
    [
      VarnodeAttach(Tagged(
        value: VarnodeAttach(
          fields: [
            Tagged(
              value: Ident("Rt"),
              tag: Span(
                src: FileId([]),
                range: (25, 27),
              ),
            ),
            Tagged(
              value: Ident("Rs"),
              tag: Span(
                src: FileId([]),
                range: (28, 30),
              ),
            ),
          ],
          registers: [
            Tagged(
              value: Ident("r0"),
              tag: Span(
                src: FileId([]),
                range: (44, 46),
              ),
            ),
            Tagged(
              value: Ident("_"),
              tag: Span(
                src: FileId([]),
                range: (47, 48),
              ),
            ),
            Tagged(
              value: Ident("r1"),
              tag: Span(
                src: FileId([]),
                range: (49, 51),
              ),
            ),
          ],
        ),
        tag: Span(
          src: FileId([]),
          range: (7, 60),
        ),
      )),
    ]
    "#);
    Ok(())
}
