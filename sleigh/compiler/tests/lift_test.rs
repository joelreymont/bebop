// use bebop_sleigh_parser::{error::*, grammar, lexer::*};
// use bebop_sleigh_util::meta::*;
// use insta::*;

// #[test]
// fn test_def_endian() -> Result<(), ParseError> {
//     let s = r"
//       define endian = big;
//     ";
//     let lexer = Lexer::new(s);
//     let parser = grammar::DefsParser::new();
//     let ast = parser.parse(FileId::empty(), lexer)?;
//     assert_ron_snapshot!(ast, @r"
//     [
//       Endian(Big),
//     ]
//     ");
//     Ok(())
// }
