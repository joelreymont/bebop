use bebop_sleigh::{error::*, grammar, lexer::*, meta::*};
use insta::*;

#[test]
fn test_endian() -> Result<(), ParseError> {
    let s = r"define endian = big;";
    let lexer = Lexer::new(s);
    let parser = grammar::DefinitionsParser::new();
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
