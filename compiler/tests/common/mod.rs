use bebop_compiler::{error::*, ir::*};
use bebop_parser::{parse, *};

pub fn parse_and_lift(s: &str) -> Result<Architecture, Error> {
    let ast = parse(DefsParserEx::new(), s).map_err(Error::ParserError)?;
    let mut arch = Architecture::new();
    arch.lift(ast)?;
    Ok(arch)
}
