use bebop_sleigh::lexer::*;
use insta::*;

#[test]
fn test_normal() -> Result<(), ()> {
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
    assert_yaml_snapshot!(tokens, @"");
    Ok(())
}
