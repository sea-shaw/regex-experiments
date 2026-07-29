package experiments.macros

import experiments.macros.catnip.r
import cats.data.Ior.Both
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class QT3TSTests extends AnyFlatSpec {
  behavior of "QT3TS Tests"

  it should "pass QT3TS tests" in {
    // val r1 = r"((((((((((a))))))))))"
    // "a" should matchPattern { case r1("a", "a", "a", "a", "a", "a", "a", "a", "a", "a") => }

    // val r2 = r"((((((((((a))))))))))\\10" // "aa" should matchPattern { case r2(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) == ("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")) }
    // val r3 = r"(((((((((a)))))))))"
    // "a" should matchPattern { case r3("a", "a", "a", "a", "a", "a", "a", "a", "a") => }

    val r4 = r"((?:aaaa|bbbb)cccc)?"
    "aaaacccc" should matchPattern { case r4(Some("aaaacccc")) => }

    val r5 = r"((?:aaaa|bbbb)cccc)?"
    "bbbbcccc" should matchPattern { case r5(Some("bbbbcccc")) => }

    // val r6 = r"((?i)a)b" // "ab" should matchPattern { case r6(g0) => assert((g0) == ("a")) }
    // val r7 = r"((?i)a)b" // "Ab" should matchPattern { case r7(g0) => assert((g0) == ("A")) }
    // val r8 = r"((?i:a))b" // "ab" should matchPattern { case r8(g0) => assert((g0) == ("a")) }
    // val r9 = r"((?i:a))b" // "Ab" should matchPattern { case r9(g0) => assert((g0) == ("A")) }
    // val r10 = r"(([a-c])b*?\\2)*" // "ababbbcbc" should matchPattern { case r10(Some(g0), Some(g1)) => assert((g0, g1) == ("cbc", "c")) }
    // val r11 = r"(([a-c])b*?\\2){3}" // "ababbbcbc" should matchPattern { case r11(g0, g1) => assert((g0, g1) == ("cbc", "c")) }
    val r12 = r"((a)(b)c)(d)"
    "abcd" should matchPattern { case r12("abc", "a", "b", "d") => }

    // TODO: What is the type of this regex?
    val r13 = r"((foo)|(bar))*"
    "foobar" should matchPattern { case r13(Some("bar", Both("foo", "bar"))) => }
    // "foobar" should matchPattern { case r13(Some(g0), Some(g1), Some(g2)) => assert((g0, g1, g2) == ("bar", "foo", "bar")) }

    val r14 = r"(.*)c(.*)"
    "abcde" should matchPattern { case r14("ab", "de") => }

    // TODO: What is the type of this regex?
    val r15 = r"(?:(f)(o)(o)|(b)(a)(r))*"
    "foobar" should matchPattern { case r15(Some(Both(("f", "o", "o"), ("b", "a", "r")))) => }
    // "foobar" should matchPattern { case r15(Some(g0), Some(g1), Some(g2), Some(g3), Some(g4), Some(g5)) => assert((g0, g1, g2, g3, g4, g5) == ("f", "o", "o", "b", "a", "r")) }

    // val r16 = r"([[:digit:]-[:alpha:]]+)" // "-" should matchPattern { case r16(g0) => assert((g0) == ("-")) }
    // val r17 = r"([[:digit:]-z]+)" // "-" should matchPattern { case r17(g0) => assert((g0) == ("-")) }
    // val r18 = r"([\\d-\\s]+)" // "-" should matchPattern { case r18(g0) => assert((g0) == ("-")) }
    // val r19 = r"([\\d-z]+)" // "-" should matchPattern { case r19(g0) => assert((g0) == ("-")) }
    // val r20 = r"([\\w:]+::)?(\\w+)$" // "abcd" should matchPattern { case r20(None, g1) => assert((null, g1) == (null, "abcd")) }
    // val r21 = r"([\\w:]+::)?(\\w+)$" // "xy:z:::abcd" should matchPattern { case r21(Some(g0), g1) => assert((g0, g1) == ("xy:z:::", "abcd")) }
    // val r22 = r"([a-c]*)\\1" // "abcabc" should matchPattern { case r22(g0) => assert((g0) == ("abc")) }
    // val r23 = r"([abc])*bcd" // "abcd" should matchPattern { case r23(Some(g0)) => assert((g0) == ("a")) }
    // val r24 = r"([abc])*d" // "abbbcd" should matchPattern { case r24(Some(g0)) => assert((g0) == ("c")) }
    // val r25 = r"([yX].|WORDS|[yX].|WORD)+S" // "WORDS" should matchPattern { case r25(g0) => assert((g0) == ("WORD")) }
    // val r26 = r"([yX].|WORDS|[yX].|WORD)S" // "WORDS" should matchPattern { case r26(g0) => assert((g0) == ("WORD")) }
    // val r27 = r"([yX].|WORDS|WORD|[xY].)+S" // "WORDS" should matchPattern { case r27(g0) => assert((g0) == ("WORD")) }
    // val r28 = r"([yX].|WORDS|WORD|[xY].)S" // "WORDS" should matchPattern { case r28(g0) => assert((g0) == ("WORD")) }
    // val r29 = r"([zx].|foo|fool|[zq].|money|parted|[yx].)$" // "fool" should matchPattern { case r29(g0) => assert((g0) == ("fool")) }
    // val r30 = r"([zx].|foo|fool|[zq].|money|parted|[yx].)+$" // "fool" should matchPattern { case r30(g0) => assert((g0) == ("fool")) }
    // val r31 = r"(\\d+\\.\\d+)" // "3.1415926" should matchPattern { case r31(g0) => assert((g0) == ("3.1415926")) }
    // val r32 = r"(\\w+:)+" // "one:" should matchPattern { case r32(g0) => assert((g0) == ("one:")) }
    // val r33 = r"(^|a)b" // "ab" should matchPattern { case r33(g0) => assert((g0) == ("a")) }

    val r34 = r"(a)?(a)+"
    "a" should matchPattern { case r34(None, "a") => }

    val r35 = r"(a)b(c)"
    "abc" should matchPattern { case r35("a", "c") => }

    val r36 = r"(a)|(b)"
    "b" should matchPattern { case r36(Right("b")) => }

    // val r37 = r"(a)|\\1" // "a" should matchPattern { case r37(Some(g0)) => assert((g0) == ("a")) }

    val r38 = r"(a+|b)*"
    "ab" should matchPattern { case r38(Some("b")) => }

    val r39 = r"(a+|b)+"
    "ab" should matchPattern { case r39("b") => }

    // val r40 = r"(a+|b){0,}" // "ab" should matchPattern { case r40(Some(g0)) => assert((g0) == ("b")) }
    // val r41 = r"(a+|b){1,}" // "ab" should matchPattern { case r41(g0) => assert((g0) == ("b")) }

    val r42 = r"(aA)*+b"
    "aAaAaAaAaAb" should matchPattern { case r42(Some("aA")) => }

    val r43 = r"(aA)++b"
    "aAaAaAaAaAb" should matchPattern { case r43("aA") => }

    val r44 = r"(aA)?+b"
    "aAb" should matchPattern { case r44(Some("aA")) => }

    // val r45 = r"(aA){1,5}+b" // "aAaAaAaAaAb" should matchPattern { case r45(g0) => assert((g0) == ("aA")) }

    val r46 = r"(aA|bB)*+b"
    "bBbBbBbBbBb" should matchPattern { case r46(Some("bB")) => }

    val r47 = r"(aA|bB)++b"
    "aAbBaAaAbBb" should matchPattern { case r47("bB") => }

    val r48 = r"(aA|bB)?+b"
    "bBb" should matchPattern { case r48(Some("bB")) => }

    // val r49 = r"(aA|bB){1,5}+b" // "bBaAbBaAbBb" should matchPattern { case r49(g0) => assert((g0) == ("bB")) }

    val r50 = r"(ab)?(ab)+"
    "ab" should matchPattern { case r50(None, "ab") => }

    val r51 = r"(abc)?(abc)+"
    "abc" should matchPattern { case r51(None, "abc") => }

    // val r52 = r"(abc)\\1" // "abcabc" should matchPattern { case r52(g0) => assert((g0) == ("abc")) }

    val r53 = r"(ab|a)b*c"
    "abc" should matchPattern { case r53("ab") => }

    val r54 = r"(ab|ab*)bc"
    "abc" should matchPattern { case r54("a") => }

    // val r55 = r"(a|(bc)){0,0}+xyz" // "xyz" should matchPattern { case r55(None, None) => assert((null, null) == (null, null)) }

    // val r56 = r"(a|(bc)){0,0}?xyz" // "xyz" should matchPattern { case r56(None, None) => assert((null, null) == (null, null)) }

    val r57 = r"(a|b|c|d|e)f"
    "ef" should matchPattern { case r57("e") => }

    // val r58 = r"(bc+d$|ef*g.|h?i(j|k))" // "effgz" should matchPattern { case r58(g0, None) => assert((g0, null) == ("effgz", null)) }
    // val r59 = r"(bc+d$|ef*g.|h?i(j|k))" // "ij" should matchPattern { case r59(g0, Some(g1)) => assert((g0, g1) == ("ij", "j")) }
    // val r60 = r"(foo[1x]|bar[2x]|baz[3x])*y" // "foo1bar2baz3y" should matchPattern { case r60(Some(g0)) => assert((g0) == ("baz3")) }
    // val r61 = r"(foo[1x]|bar[2x]|baz[3x])+y" // "foo1bar2baz3y" should matchPattern { case r61(g0) => assert((g0) == ("baz3")) }
    // val r62 = r"(foo|fool|[zx].|money|parted)$" // "fool" should matchPattern { case r62(g0) => assert((g0) == ("fool")) }
    // val r63 = r"(foo|fool|[zx].|money|parted)+$" // "fool" should matchPattern { case r63(g0) => assert((g0) == ("fool")) }
    // val r64 = r"(foo|fool|money|parted)$" // "fool" should matchPattern { case r64(g0) => assert((g0) == ("fool")) }
    // val r65 = r"(foo|fool|x.|money|parted)$" // "fool" should matchPattern { case r65(g0) => assert((g0) == ("fool")) }
    // val r66 = r"(q1|.)*(q2|.)*(x(a|bc)*y){2,3}" // "xayxay" should matchPattern { case r66(None, None, g2, Some(g3)) => assert((null, null, g2, g3) == (null, null, "xay", "a")) }
    // val r67 = r"(q1|.)*(q2|.)*(x(a|bc)*y){2,}" // "xayxay" should matchPattern { case r67(None, None, g2, Some(g3)) => assert((null, null, g2, g3) == (null, null, "xay", "a")) }

    // val r68 = r"(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z" // "zzzzzzzzzzzzzzzz-xayxayxayxayZ" should matchPattern { case r68(Some(g0), None, g2, Some(g3)) => assert((g0, null, g2, g3) == ("z", null, "xay", "a")) }

    val r69 = r"(WORDS|WORD)S"
    "WORDS" should matchPattern { case r69("WORD") => }

    val r70 = r"(WORDS|WORLD|WORD)+S"
    "WORDS" should matchPattern { case r70("WORD") => }

    val r71 = r"(WORDS|WORLD|WORD)S"
    "WORDS" should matchPattern { case r71("WORD") => }

    // val r72 = r"(x.|foo|fool|x.|money|parted|y.)$" // "fool" should matchPattern { case r72(g0) => assert((g0) == ("fool")) }

    val r73 = r"(X.|WORDS|WORD|Y.)S"
    "WORDS" should matchPattern { case r73("WORD") => }

    val r74 = r"(X.|WORDS|X.|WORD)S"
    "WORDS" should matchPattern { case r74("WORD") => }

    // val r75 = r"(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*" // "xyzQzWlongishoverblownW" should matchPattern { case r75(Some(g0), Some(g1)) => assert((g0, g1) == ("zW", "overblownW")) }
    // val r76 = r"(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+" // "xyzQzWlongishoverblownW" should matchPattern { case r76(Some(g0), Some(g1)) => assert((g0, g1) == ("zW", "overblownW")) }
    // val r77 = r"(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+" // "xyzQzWlongishoverblownW" should matchPattern { case r77(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
    // val r78 = r"(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++" // "xyzQzWlongishoverblownW" should matchPattern { case r78(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
    // val r79 = r"(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}" // "xyzQzWlongishoverblownW" should matchPattern { case r79(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
    // val r80 = r"(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+" // "xyzQzWlongishoverblownW" should matchPattern { case r80(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
    // val r81 = r".*?(?:(\\w)|(\\w))x" // "abx" should matchPattern { case r81(Some(g0), None) => assert((g0, null) == ("b", null)) }
    // val r82 = r"2(]*)?$\\1" // "2" should matchPattern { case r82(Some(g0)) => assert((g0) == ("")) }
    // val r83 = r"\\((.*), (.*)\\)" // "(a, b)" should matchPattern { case r83(g0, g1) => assert((g0, g1) == ("a", "b")) }
    // val r84 = r"^((?:aa)*)(?:X+((?:\\d+|-)(?:X+(.+))?))?$" // "aaaaX5" should matchPattern { case r84(g0, Some(g1), None) => assert((g0, g1, null) == ("aaaa", "5", null)) }
    // val r85 = r"^((a|b)+)*ax" // "aax" should matchPattern { case r85(Some(g0), Some(g1)) => assert((g0, g1) == ("a", "a")) }
    // val r86 = r"^((a|bc)+)*ax" // "aax" should matchPattern { case r86(Some(g0), Some(g1)) => assert((g0, g1) == ("a", "a")) }
    // val r87 = r"^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$" // "text|\'sec\'" should matchPattern { case r87(g0, g1) => assert((g0, g1) == ("text", "sec")) }
    // val r88 = r"^(.+)?B" // "AB" should matchPattern { case r88(Some(g0)) => assert((g0) == ("A")) }
    // val r89 = r"^(.,){2}c" // "a,b,c" should matchPattern { case r89(g0) => assert((g0) == ("b,")) }
    // val r90 = r"^(0+)?(?:x(1))?" // "x1" should matchPattern { case r90(None, Some(g1)) => assert((null, g1) == (null, "1")) }
    // val r91 = r"^(?:(\\d)x)?\\d$" // "1" should matchPattern { case r91(None) => assert((null) == (null)) }
    // val r92 = r"^(?:(X)?(\\d)|(X)?(\\d\\d))$" // "X12" should matchPattern { case r92(None, None, Some(g2), Some(g3)) => assert((null, null, g2, g3) == (null, null, "X", "12")) }
    // val r93 = r"^(?:(XX)?(\\d)|(XX)?(\\d\\d))$" // "XX12" should matchPattern { case r93(None, None, Some(g2), Some(g3)) => assert((null, null, g2, g3) == (null, null, "XX", "12")) }
    // val r94 = r"^(?:f|o|b){2,3}?((?:b|a|r)+)\\1$" // "foobarbar" should matchPattern { case r94(g0) => assert((g0) == ("bar")) }
    // val r95 = r"^(?:f|o|b){2,3}?((?:b|a|r)+?)\\1$" // "foobarbar" should matchPattern { case r95(g0) => assert((g0) == ("bar")) }
    // val r96 = r"^(?:f|o|b){2,3}?(.+)\\1$" // "foobarbar" should matchPattern { case r96(g0) => assert((g0) == ("bar")) }
    // val r97 = r"^(?:f|o|b){2,3}?(.+?)\\1$" // "foobarbar" should matchPattern { case r97(g0) => assert((g0) == ("bar")) }
    // val r98 = r"^(?:f|o|b){3,4}((?:b|a|r)+)\\1$" // "foobarbar" should matchPattern { case r98(g0) => assert((g0) == ("bar")) }
    // val r99 = r"^(?:f|o|b){3,4}((?:b|a|r)+?)\\1$" // "foobarbar" should matchPattern { case r99(g0) => assert((g0) == ("bar")) }
    // val r100 = r"^(?:f|o|b){3,4}(.+)\\1$" // "foobarbar" should matchPattern { case r100(g0) => assert((g0) == ("bar")) }
    // val r101 = r"^(?:f|o|b){3,4}(.+?)\\1$" // "foobarbar" should matchPattern { case r101(g0) => assert((g0) == ("bar")) }
    // val r102 = r"^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?" // "012cxx0190" should matchPattern { case r102(g0, None, Some(g2)) => assert((g0, null, g2) == ("012c", null, "0190")) }
    // val r103 = r"^([^,]*,){0,3}d" // "aaa,b,c,d" should matchPattern { case r103(Some(g0)) => assert((g0) == ("c,")) }
    // val r104 = r"^([^,]*,){2}c" // "a,b,c" should matchPattern { case r104(g0) => assert((g0) == ("b,")) }
    // val r105 = r"^([^,]*,){3,}d" // "aaa,b,c,d" should matchPattern { case r105(g0) => assert((g0) == ("c,")) }
    // val r106 = r"^([^,]*,){3}d" // "aaa,b,c,d" should matchPattern { case r106(g0) => assert((g0) == ("c,")) }
    // val r107 = r"^([^,]{0,3},){0,3}d" // "aaa,b,c,d" should matchPattern { case r107(Some(g0)) => assert((g0) == ("c,")) }
    // val r108 = r"^([^,]{0,3},){3,}d" // "aaa,b,c,d" should matchPattern { case r108(g0) => assert((g0) == ("c,")) }
    // val r109 = r"^([^,]{0,3},){3}d" // "aaa,b,c,d" should matchPattern { case r109(g0) => assert((g0) == ("c,")) }
    // val r110 = r"^([^,]{1,3},){0,3}d" // "aaa,b,c,d" should matchPattern { case r110(Some(g0)) => assert((g0) == ("c,")) }
    // val r111 = r"^([^,]{1,3},){3,}d" // "aaa,b,c,d" should matchPattern { case r111(g0) => assert((g0) == ("c,")) }
    // val r112 = r"^([^,]{1,3},){3}d" // "aaa,b,c,d" should matchPattern { case r112(g0) => assert((g0) == ("c,")) }
    // val r113 = r"^([^,]{1,},){0,3}d" // "aaa,b,c,d" should matchPattern { case r113(Some(g0)) => assert((g0) == ("c,")) }
    // val r114 = r"^([^,]{1,},){3,}d" // "aaa,b,c,d" should matchPattern { case r114(g0) => assert((g0) == ("c,")) }
    // val r115 = r"^([^,]{1,},){3}d" // "aaa,b,c,d" should matchPattern { case r115(g0) => assert((g0) == ("c,")) }
    // val r116 = r"^([^a-z])|(\\^)$" // "." should matchPattern { case r116(Some(g0), None) => assert((g0, null) == (".", null)) }
    // val r117 = r"^([a]{1})*$" // "aa" should matchPattern { case r117(Some(g0)) => assert((g0) == ("a")) }
    // val r118 = r"^([ab]*?)(b)?(c)$" // "abac" should matchPattern { case r118(g0, None, g2) => assert((g0, null, g2) == ("aba", null, "c")) }
    // val r119 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):" // "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r119(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    // val r120 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):" // "ZEQQQX:" should matchPattern { case r120(g0) => assert((g0) == ("ZEQQQX")) }
    // val r121 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):" // "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r121(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    // val r122 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):" // "ZEQQQX:" should matchPattern { case r122(g0) => assert((g0) == ("ZEQQQX")) }
    // val r123 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):" // "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r123(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    // val r124 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):" // "ZEQQQX:" should matchPattern { case r124(g0) => assert((g0) == ("ZEQQQX")) }
    // val r125 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):" // "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r125(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    // val r126 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):" // "ZEQQQX:" should matchPattern { case r126(g0) => assert((g0) == ("ZEQQQX")) }
    // val r127 = r"^(a(b)?)+$" // "aba" should matchPattern { case r127(g0, Some(g1)) => assert((g0, g1) == ("a", "b")) }
    // val r128 = r"^(a)?a$" // "a" should matchPattern { case r128(None) => assert((null) == (null)) }
    // val r129 = r"^(a+)*ax" // "aax" should matchPattern { case r129(Some(g0)) => assert((g0) == ("a")) }
    // val r130 = r"^(a\\1?)(a\\1?)(a\\2?)(a\\3?)$" // "aaaaaa" should matchPattern { case r130(g0, g1, g2, g3) => assert((g0, g1, g2, g3) == ("a", "aa", "a", "aa")) }
    // val r131 = r"^(a\\1?){4}$" // "aaaaaa" should matchPattern { case r131(g0) => assert((g0) == ("aa")) }
    // val r132 = r"^(a\\1?){4}$" // "aaaaaaaaaa" should matchPattern { case r132(g0) => assert((g0) == ("aaaa")) }
    // val r133 = r"^(aa(bb)?)+$" // "aabbaa" should matchPattern { case r133(g0, Some(g1)) => assert((g0, g1) == ("aa", "bb")) }
    // val r134 = r"^(b+?|a){1,2}c" // "bbbac" should matchPattern { case r134(g0) => assert((g0) == ("a")) }
    // val r135 = r"^(b+?|a){1,2}c" // "bbbbac" should matchPattern { case r135(g0) => assert((g0) == ("a")) }
    // val r136 = r"^(foo|)bar$" // "bar" should matchPattern { case r136(g0) => assert((g0) == ("")) }
    // val r137 = r"^(foo||baz)bar$" // "bar" should matchPattern { case r137(g0) => assert((g0) == ("")) }
    // val r138 = r"^(foo||baz)bar$" // "bazbar" should matchPattern { case r138(g0) => assert((g0) == ("baz")) }
    // val r139 = r"^(foo||baz)bar$" // "foobar" should matchPattern { case r139(g0) => assert((g0) == ("foo")) }
    // val r140 = r"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):" // "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r140(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    // val r141 = r"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):" // "ZEQQQX:" should matchPattern { case r141(g0) => assert((g0) == ("ZEQQQX")) }
    // val r142 = r"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):" // "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r142(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    // val r143 = r"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):" // "ZEQQQX:" should matchPattern { case r143(g0) => assert((g0) == ("ZEQQQX")) }
    // val r144 = r"^.{2,3}?((?:b|a|r)+)\\1$" // "foobarbar" should matchPattern { case r144(g0) => assert((g0) == ("bar")) }
    // val r145 = r"^.{2,3}?((?:b|a|r)+?)\\1$" // "foobarbar" should matchPattern { case r145(g0) => assert((g0) == ("bar")) }
    // val r146 = r"^.{2,3}?(.+)\\1$" // "foobarbar" should matchPattern { case r146(g0) => assert((g0) == ("bar")) }
    // val r147 = r"^.{2,3}?(.+?)\\1$" // "foobarbar" should matchPattern { case r147(g0) => assert((g0) == ("bar")) }
    // val r148 = r"^.{3,4}((?:b|a|r)+)\\1$" // "foobarbar" should matchPattern { case r148(g0) => assert((g0) == ("bar")) }
    // val r149 = r"^.{3,4}((?:b|a|r)+?)\\1$" // "foobarbar" should matchPattern { case r149(g0) => assert((g0) == ("bar")) }
    // val r150 = r"^.{3,4}(.+)\\1$" // "foobarbar" should matchPattern { case r150(g0) => assert((g0) == ("bar")) }
    // val r151 = r"^.{3,4}(.+?)\\1$" // "foobarbar" should matchPattern { case r151(g0) => assert((g0) == ("bar")) }
    // val r152 = r"^m?(\\d)(.*)\\1$" // "5b5" should matchPattern { case r152(g0, g1) => assert((g0, g1) == ("5", "b")) }
    // val r153 = r"^m?(\\D)(.*)\\1$" // "aba" should matchPattern { case r153(g0, g1) => assert((g0, g1) == ("a", "b")) }
    // val r154 = r"^m?(\\S)(.*)\\1$" // "aba" should matchPattern { case r154(g0, g1) => assert((g0, g1) == ("a", "b")) }
    // val r155 = r"^m?(\\W)(.*)\\1$" // ":b:" should matchPattern { case r155(g0, g1) => assert((g0, g1) == (":", "b")) }
    // val r156 = r"^m?(\\w)(.*)\\1$" // "aba" should matchPattern { case r156(g0, g1) => assert((g0, g1) == ("a", "b")) }

    // val r157 = r"a(?:b|(c|e){1,2}?|d)+?(.)" // "ace" should matchPattern { case r157(Some(g0), g1) => assert((g0, g1) == ("c", "e")) }

    val r158 = r"a(?:b|c|d)(.)"
    "ace" should matchPattern { case r158("e") => }

    val r159 = r"a(?:b|c|d)*(.)"
    "ace" should matchPattern { case r159("e") => }

    val r160 = r"a(?:b|c|d)+(.)"
    "acdbcdbe" should matchPattern { case r160("e") => }

    val r161 = r"a(?:b|c|d)+?(.)"
    "acdbcdbe" should matchPattern { case r161("e") => }

    val r162 = r"a(?:b|c|d)+?(.)"
    "ace" should matchPattern { case r162("e") => }

    // val r163 = r"a(?:b|c|d){5,6}(.)" // "acdbcdbe" should matchPattern { case r163(g0) => assert((g0) == ("e")) }
    // val r164 = r"a(?:b|c|d){5,6}?(.)" // "acdbcdbe" should matchPattern { case r164(g0) => assert((g0) == ("e")) }
    // val r165 = r"a(?:b|c|d){5,7}(.)" // "acdbcdbe" should matchPattern { case r165(g0) => assert((g0) == ("e")) }
    // val r166 = r"a(?:b|c|d){5,7}?(.)" // "acdbcdbe" should matchPattern { case r166(g0) => assert((g0) == ("e")) }
    // val r167 = r"a(?:b|c|d){6,7}(.)" // "acdbcdbe" should matchPattern { case r167(g0) => assert((g0) == ("e")) }
    // val r168 = r"a(?:b|c|d){6,7}?(.)" // "acdbcdbe" should matchPattern { case r168(g0) => assert((g0) == ("e")) }
    // val r169 = r"a([bc]*)(c*d)" // "abcd" should matchPattern { case r169(g0, g1) => assert((g0, g1) == ("bc", "d")) }
    // val r170 = r"a([bc]*)(c+d)" // "abcd" should matchPattern { case r170(g0, g1) => assert((g0, g1) == ("b", "cd")) }
    // val r171 = r"a([bc]*)c*" // "abc" should matchPattern { case r171(g0) => assert((g0) == ("bc")) }

    // val r172 = r"a([bc]+)(c*d)" // "abcd" should matchPattern { case r172(g0, g1) => assert((g0, g1) == ("bc", "d")) }

    val r173 = r"a(bc)d"
    "abcd" should matchPattern { case r173("bc") => }

    val r174 = r"foo(aA)*+b"
    "fooaAaAaAaAaAb" should matchPattern { case r174(Some("aA")) => }

    val r175 = r"foo(aA)++b"
    "fooaAaAaAaAaAb" should matchPattern { case r175("aA") => }

    val r176 = r"foo(aA)?+b"
    "fooaAb" should matchPattern { case r176(Some("aA")) => }

    // val r177 = r"foo(aA){1,5}+b" // "fooaAaAaAaAaAb" should matchPattern { case r177(g0) => assert((g0) == ("aA")) }

    val r178 = r"foo(aA|bB)*+b"
    "foobBbBaAaAaAb" should matchPattern { case r178(Some("aA")) => }

    val r179 = r"foo(aA|bB)++b"
    "foobBaAbBaAbBb" should matchPattern { case r179("bB") => }

    val r180 = r"foo(aA|bB)?+b"
    "foobBb" should matchPattern { case r180(Some("bB")) => }

    // val r181 = r"foo(aA|bB){1,5}+b" // "foobBaAaAaAaAb" should matchPattern { case r181(g0) => assert((g0) == ("aA")) }
    // val r182 = r"X(\\w+)(?=\\s)|X(\\w+)" // "Xab" should matchPattern { case r182(None, Some(g1)) => assert((null, g1) == (null, "ab")) }
    // val r183 = r"x(~~)*(?:(?:F)?)?" // "x~~" should matchPattern { case r183(Some(g0)) => assert((g0) == ("~~")) }
  }
}
