use bebop_sleigh::{error::*, lexer::*};
use insta::*;

#[test]
fn test_normal() -> Result<(), ParseError> {
    let s = r"
        123 0b0111 0B100 0x123 0XABC foo
        is IS
        if IF
        register REGISTER
        alignment ALIGNMENT
        attach ATTACH
        big BIG
        default DEFAULT
        little LITTLE
        build BUILD
        call CALL
        dec DEC
        define DEFINE
        endian ENDIAN
        export EXPORT
        goto GOTO
        hex HEX
        local LOCAL
        macro MACRO
        pcodeop PCodeOp
        return RETURN
        signed SIGNED
        token TOKEN
        unimpl UNIMPL
        ...{}[]():,!~;=<>
        == != <= >= || && ^^ | & ^
        f< f> f== f!= f<= f>=
        s< s> s<= s>=
        $or $and $xor
        << >> s>> s<<
        + - * / % f+ f- f* f/ s/ s%
        # should be ignored
    ";
    let lexer = Lexer::new(s);
    let tokens: Vec<_> = lexer.collect::<Result<_, _>>()?;
    assert_ron_snapshot!(tokens, @r#"
    [
      (9, Normal(DecInt(123)), 12),
      (13, Normal(BinInt(7)), 19),
      (20, Normal(BinInt(4)), 25),
      (26, Normal(HexInt(291)), 31),
      (32, Normal(HexInt(2748)), 37),
      (38, Normal(Ident("foo")), 41),
      (50, Normal(Is), 52),
      (53, Normal(Is), 55),
      (64, Normal(If), 66),
      (67, Normal(If), 69),
      (78, Normal(Register), 86),
      (87, Normal(Register), 95),
      (104, Normal(Alignment), 113),
      (114, Normal(Alignment), 123),
      (132, Normal(Attach), 138),
      (139, Normal(Attach), 145),
      (154, Normal(Big), 157),
      (158, Normal(Big), 161),
      (170, Normal(Default), 177),
      (178, Normal(Default), 185),
      (194, Normal(Little), 200),
      (201, Normal(Little), 207),
      (216, Normal(Build), 221),
      (222, Normal(Build), 227),
      (236, Normal(Call), 240),
      (241, Normal(Call), 245),
      (254, Normal(Dec), 257),
      (258, Normal(Dec), 261),
      (270, Normal(Define), 276),
      (277, Normal(Define), 283),
      (292, Normal(Endian), 298),
      (299, Normal(Endian), 305),
      (314, Normal(Export), 320),
      (321, Normal(Export), 327),
      (336, Normal(Goto), 340),
      (341, Normal(Goto), 345),
      (354, Normal(Hex), 357),
      (358, Normal(Hex), 361),
      (370, Normal(Local), 375),
      (376, Normal(Local), 381),
      (390, Normal(Macro), 395),
      (396, Normal(Macro), 401),
      (410, Normal(PCodeOp), 417),
      (418, Normal(PCodeOp), 425),
      (434, Normal(Return), 440),
      (441, Normal(Return), 447),
      (456, Normal(Signed), 462),
      (463, Normal(Signed), 469),
      (478, Normal(Token), 483),
      (484, Normal(Token), 489),
      (498, Normal(Unimpl), 504),
      (505, Normal(Unimpl), 511),
      (520, Normal(Ellipsis), 523),
      (523, Normal(LBrace), 524),
      (524, Normal(RBrace), 525),
      (525, Normal(LBracket), 526),
      (526, Normal(RBracket), 527),
      (527, Normal(LParen), 528),
      (528, Normal(RParen), 529),
      (529, Normal(Colon), 530),
      (530, Normal(Comma), 531),
      (531, Normal(Bang), 532),
      (532, Normal(Tilde), 533),
      (533, Normal(Semi), 534),
      (534, Normal(Assign), 535),
      (535, Normal(LT), 536),
      (536, Normal(GT), 537),
      (546, Normal(EQ), 548),
      (549, Normal(NE), 551),
      (552, Normal(LE), 554),
      (555, Normal(GE), 557),
      (558, Normal(Or), 560),
      (561, Normal(And), 563),
      (564, Normal(Xor), 566),
      (567, Normal(Pipe), 568),
      (569, Normal(Ampersand), 570),
      (571, Normal(Caret), 572),
      (581, Normal(FLT), 583),
      (584, Normal(FGT), 586),
      (587, Normal(FEQ), 590),
      (591, Normal(FNE), 594),
      (595, Normal(FLE), 598),
      (599, Normal(FGE), 602),
      (611, Normal(SLT), 613),
      (614, Normal(SGT), 616),
      (617, Normal(SLE), 620),
      (621, Normal(SGE), 624),
      (633, Normal(SpecOr), 636),
      (637, Normal(SpecAnd), 641),
      (642, Normal(SpecXor), 646),
      (655, Normal(LShift), 657),
      (658, Normal(RShift), 660),
      (661, Normal(SRShift), 664),
      (665, Normal(SLShift), 668),
      (677, Normal(Plus), 678),
      (679, Normal(Minus), 680),
      (681, Normal(Star), 682),
      (683, Normal(Slash), 684),
      (685, Normal(Percent), 686),
      (687, Normal(FPlus), 689),
      (690, Normal(FMinus), 692),
      (693, Normal(FMul), 695),
      (696, Normal(FDiv), 698),
      (699, Normal(SDiv), 701),
      (702, Normal(SMod), 704),
    ]
    "#);
    Ok(())
}

#[test]
fn test_display() -> Result<(), ParseError> {
    let s = r#"foo^[Baz], bar bar1"#;
    let mut lexer = Lexer::new(s);
    lexer.switch_to_display();
    let tokens: Vec<_> = lexer.collect::<Result<_, _>>()?;
    assert_ron_snapshot!(tokens, @r#"
    [
      (0, Display(Ident("foo")), 3),
      (3, Display(Caret), 4),
      (4, Display(Text("[Baz],")), 10),
      (10, Display(Whitespace), 11),
      (11, Display(Ident("bar")), 14),
      (14, Display(Whitespace), 15),
      (15, Display(Ident("bar1")), 19),
    ]
    "#);
    Ok(())
}
