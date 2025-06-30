use bebop_sleigh::{error::*, lexer::*};
use insta::*;

#[test]
fn test_normal() -> Result<(), ParseError> {
    let s = r"
        123 0b0111 0B100 0x123 0XABC foo
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
        ...[]():,!~;=<>
        == != <= >= || && ^^ | & ^
        f< f> f== f!= f<= f>=
        s< s> s<= s>=
        $or $and $xor
        << >> s>> s<<
        + - * / % f+ f- f* f/ s/ s%
        # should be ignored
        {}
        is IS
        unimpl UNIMPL
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
      (50, Normal(If), 52),
      (53, Normal(If), 55),
      (64, Normal(Register), 72),
      (73, Normal(Register), 81),
      (90, Normal(Alignment), 99),
      (100, Normal(Alignment), 109),
      (118, Normal(Attach), 124),
      (125, Normal(Attach), 131),
      (140, Normal(Big), 143),
      (144, Normal(Big), 147),
      (156, Normal(Default), 163),
      (164, Normal(Default), 171),
      (180, Normal(Little), 186),
      (187, Normal(Little), 193),
      (202, Normal(Build), 207),
      (208, Normal(Build), 213),
      (222, Normal(Call), 226),
      (227, Normal(Call), 231),
      (240, Normal(Dec), 243),
      (244, Normal(Dec), 247),
      (256, Normal(Define), 262),
      (263, Normal(Define), 269),
      (278, Normal(Endian), 284),
      (285, Normal(Endian), 291),
      (300, Normal(Export), 306),
      (307, Normal(Export), 313),
      (322, Normal(Goto), 326),
      (327, Normal(Goto), 331),
      (340, Normal(Hex), 343),
      (344, Normal(Hex), 347),
      (356, Normal(Local), 361),
      (362, Normal(Local), 367),
      (376, Normal(Macro), 381),
      (382, Normal(Macro), 387),
      (396, Normal(PCodeOp), 403),
      (404, Normal(PCodeOp), 411),
      (420, Normal(Return), 426),
      (427, Normal(Return), 433),
      (442, Normal(Signed), 448),
      (449, Normal(Signed), 455),
      (464, Normal(Token), 469),
      (470, Normal(Token), 475),
      (484, Normal(Ellipsis), 487),
      (487, Normal(LBracket), 488),
      (488, Normal(RBracket), 489),
      (489, Normal(LParen), 490),
      (490, Normal(RParen), 491),
      (491, Normal(Colon), 492),
      (492, Normal(Comma), 493),
      (493, Normal(Bang), 494),
      (494, Normal(Tilde), 495),
      (495, Normal(Semi), 496),
      (496, Normal(Assign), 497),
      (497, Normal(LT), 498),
      (498, Normal(GT), 499),
      (508, Normal(EQ), 510),
      (511, Normal(NE), 513),
      (514, Normal(LE), 516),
      (517, Normal(GE), 519),
      (520, Normal(Or), 522),
      (523, Normal(And), 525),
      (526, Normal(Xor), 528),
      (529, Normal(Pipe), 530),
      (531, Normal(Ampersand), 532),
      (533, Normal(Caret), 534),
      (543, Normal(FLT), 545),
      (546, Normal(FGT), 548),
      (549, Normal(FEQ), 552),
      (553, Normal(FNE), 556),
      (557, Normal(FLE), 560),
      (561, Normal(FGE), 564),
      (573, Normal(SLT), 575),
      (576, Normal(SGT), 578),
      (579, Normal(SLE), 582),
      (583, Normal(SGE), 586),
      (595, Normal(SpecOr), 598),
      (599, Normal(SpecAnd), 603),
      (604, Normal(SpecXor), 608),
      (617, Normal(LShift), 619),
      (620, Normal(RShift), 622),
      (623, Normal(SRShift), 626),
      (627, Normal(SLShift), 630),
      (639, Normal(Plus), 640),
      (641, Normal(Minus), 642),
      (643, Normal(Star), 644),
      (645, Normal(Slash), 646),
      (647, Normal(Percent), 648),
      (649, Normal(FPlus), 651),
      (652, Normal(FMinus), 654),
      (655, Normal(FMul), 657),
      (658, Normal(FDiv), 660),
      (661, Normal(SDiv), 663),
      (664, Normal(SMod), 666),
      (703, Normal(LBrace), 704),
      (704, Normal(RBrace), 705),
      (706, Display(Whitespace), 707),
      (707, Display(Whitespace), 708),
      (708, Display(Whitespace), 709),
      (709, Display(Whitespace), 710),
      (710, Display(Whitespace), 711),
      (711, Display(Whitespace), 712),
      (712, Display(Whitespace), 713),
      (713, Display(Whitespace), 714),
      (714, Display(Is), 716),
      (717, Normal(Is), 719),
      (728, Normal(Unimpl), 734),
      (734, Display(Whitespace), 735),
      (735, Display(Ident("UNIMPL")), 741),
      (742, Display(Whitespace), 743),
      (743, Display(Whitespace), 744),
      (744, Display(Whitespace), 745),
      (745, Display(Whitespace), 746),
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
