use bebop_sleigh::{error::*, grammar, lexer::*, meta::*};
use insta::*;

#[test]
fn test_endian() -> Result<(), ParseError> {
    let s = r"define endian = big;";
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let ast = parser.parse(FileId::empty(), lexer)?;
    assert_ron_snapshot!(ast, @r"
    [
      Endian(Tagged(
        value: Big,
        tag: Span(
          src: FileId([]),
          range: (0, 19),
        ),
      )),
    ]
    ");
    Ok(())
}

#[test]
fn test_alignment() -> Result<(), ParseError> {
    let s = r"define alignment = 0x200;";
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let ast = parser.parse(FileId::empty(), lexer)?;
    assert_ron_snapshot!(ast, @r"
    [
      Alignment(Tagged(
        value: 512,
        tag: Span(
          src: FileId([]),
          range: (0, 24),
        ),
      )),
    ]
    ");
    Ok(())
}

#[test]
fn test_space1() -> Result<(), ParseError> {
    let s = r"define space ram type=ram_space size=4 wordsize=1 default;";
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let ast = parser.parse(FileId::empty(), lexer)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Space(Tagged(
        value: Space(
          id: Tagged(
            value: Ident("ram"),
            tag: Span(
              src: FileId([]),
              range: (13, 16),
            ),
          ),
          kind: Tagged(
            value: Ram,
            tag: Span(
              src: FileId([]),
              range: (22, 31),
            ),
          ),
          size: 4,
          word_size: 1,
          is_default: true,
        ),
        tag: Span(
          src: FileId([]),
          range: (0, 57),
        ),
      )),
    ]
    "#);
    Ok(())
}

#[test]
fn test_space2() -> Result<(), ParseError> {
    let s = r"define space register type=register_space size=4;";
    let lexer = Lexer::new(s);
    let parser = grammar::DefsParser::new();
    let ast = parser.parse(FileId::empty(), lexer)?;
    assert_ron_snapshot!(ast, @r#"
    [
      Space(Tagged(
        value: Space(
          id: Tagged(
            value: Ident("register"),
            tag: Span(
              src: FileId([]),
              range: (13, 21),
            ),
          ),
          kind: Tagged(
            value: Register,
            tag: Span(
              src: FileId([]),
              range: (27, 41),
            ),
          ),
          size: 4,
          word_size: 1,
          is_default: true,
        ),
        tag: Span(
          src: FileId([]),
          range: (0, 48),
        ),
      )),
    ]
    "#);
    Ok(())
}
