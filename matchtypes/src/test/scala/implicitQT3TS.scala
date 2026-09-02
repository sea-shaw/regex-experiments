// package experiments.matchtypes

// import experiments.matchtypes.implicitsearch.Regex
// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.should.Matchers.{matchPattern, should}

// class ImplicitSearchQT3TSTests extends AnyFlatSpec {
//   behavior of "Implicit Search Match Type QT3TS Tests"

//   it should "pass QT3TS tests" in {
//     val r1 = Regex("((((((((((a))))))))))")
//     "a" should matchPattern { case r1("a", "a", "a", "a", "a", "a", "a", "a", "a", "a") => }

//     val r2 = Regex("((((((((((a))))))))))\\10")
//     "aa" should matchPattern { case r2("a", "a", "a", "a", "a", "a", "a", "a", "a", "a") => }

//     val r3 = Regex("(((((((((a)))))))))")
//     "a" should matchPattern { case r3("a", "a", "a", "a", "a", "a", "a", "a", "a") => }

//     val r4 = Regex("((?:aaaa|bbbb)cccc)?")
//     "aaaacccc" should matchPattern { case r4(Some("aaaacccc")) => }

//     val r5 = Regex("((?:aaaa|bbbb)cccc)?")
//     "bbbbcccc" should matchPattern { case r5(Some("bbbbcccc")) => }

//     val r6 = Regex("((?i)a)b")
//     "ab" should matchPattern { case r6("a") => }

//     val r7 = Regex("((?i)a)b")
//     "Ab" should matchPattern { case r7("A") => }

//     val r8 = Regex("((?i:a))b")
//     "ab" should matchPattern { case r8("a") => }

//     val r9 = Regex("((?i:a))b")
//     "Ab" should matchPattern { case r9("A") => }

//     val r10 = Regex("(([a-c])b*?\\2)*")
//     "ababbbcbc" should matchPattern { case r10(Some("cbc", "c")) => }

//     val r11 = Regex("(([a-c])b*?\\2){3}")
//     "ababbbcbc" should matchPattern { case r11("cbc", "c") => }

//     val r12 = Regex("((a)(b)c)(d)")
//     "abcd" should matchPattern { case r12("abc", "a", "b", "d") => }

//     // TODO: What is the type of this regex?
//     // val r13 = Regex("((foo)|(bar))*")
//     // "foobar" should matchPattern { case r13(Some(g0), Some(g1), Some(g2)) => assert((g0, g1, g2) == ("bar", "foo", "bar")) }

//     val r14 = Regex("(.*)c(.*)")
//     "abcde" should matchPattern { case r14("ab", "de") => }

//     // TODO: What is the type of this regex?
//     // val r15 = Regex("(?:(f)(o)(o)|(b)(a)(r))*")
//     // "foobar" should matchPattern { case r15(Some(g0), Some(g1), Some(g2), Some(g3), Some(g4), Some(g5)) => assert((g0, g1, g2, g3, g4, g5) == ("f", "o", "o", "b", "a", "r")) }

//     val r16 = Regex("([[:digit:]-[:alpha:]]+)")
//     "-" should matchPattern { case r16("-") => }

//     val r17 = Regex("([[:digit:]-z]+)")
//     "-" should matchPattern { case r17("-") => }

//     val r18 = Regex("([\\d-\\s]+)")
//     "-" should matchPattern { case r18("-") => }

//     val r19 = Regex("([\\d-z]+)")
//     "-" should matchPattern { case r19("-") => }

//     val r20 = Regex("([\\w:]+::)?(\\w+)$")
//     "abcd" should matchPattern { case r20(None, "abcd") => }

//     val r21 = Regex("([\\w:]+::)?(\\w+)$")
//     "xy:z:::abcd" should matchPattern { case r21(Some("xy:z:::"), "abcd") => }

//     val r22 = Regex("([a-c]*)\\1")
//     "abcabc" should matchPattern { case r22("abc") => }

//     val r23 = Regex("([abc])*bcd")
//     "abcd" should matchPattern { case r23(Some("a")) => }

//     val r24 = Regex("([abc])*d")
//     "abbbcd" should matchPattern { case r24(Some("c")) => }

//     val r25 = Regex("([yX].|WORDS|[yX].|WORD)+S")
//     "WORDS" should matchPattern { case r25("WORD") => }

//     val r26 = Regex("([yX].|WORDS|[yX].|WORD)S")
//     "WORDS" should matchPattern { case r26("WORD") => }

//     val r27 = Regex("([yX].|WORDS|WORD|[xY].)+S")
//     "WORDS" should matchPattern { case r27("WORD") => }

//     val r28 = Regex("([yX].|WORDS|WORD|[xY].)S")
//     "WORDS" should matchPattern { case r28("WORD") => }

//     val r29 = Regex("([zx].|foo|fool|[zq].|money|parted|[yx].)$")
//     "fool" should matchPattern { case r29("fool") => }

//     val r30 = Regex("([zx].|foo|fool|[zq].|money|parted|[yx].)+$")
//     "fool" should matchPattern { case r30("fool") => }

//     val r31 = Regex("(\\d+\\.\\d+)")
//     "3.1415926" should matchPattern { case r31("3.1415926") => }

//     val r32 = Regex("(\\w+:)+")
//     "one:" should matchPattern { case r32("one:") => }

//     val r33 = Regex("(^|a)b")
//     "ab" should matchPattern { case r33("a") => }

//     val r34 = Regex("(a)?(a)+")
//     "a" should matchPattern { case r34(None, "a") => }

//     val r35 = Regex("(a)b(c)")
//     "abc" should matchPattern { case r35("a", "c") => }

//     val r36 = Regex("(a)|(b)")
//     "b" should matchPattern { case r36(Right("b")) => }

//     val r37 = Regex("(a)|\\1")
//     "a" should matchPattern { case r37(Left("a")) => }

//     val r38 = Regex("(a+|b)*")
//     "ab" should matchPattern { case r38(Some("b")) => }

//     val r39 = Regex("(a+|b)+")
//     "ab" should matchPattern { case r39("b") => }

//     // val r40 = Regex("(a+|b){0,}")
//     // "ab" should matchPattern { case r40(Some("b")) => }

//     val r41 = Regex("(a+|b){1,}")
//     "ab" should matchPattern { case r41("b") => }

//     val r42 = Regex("(aA)*+b")
//     "aAaAaAaAaAb" should matchPattern { case r42(Some("aA")) => }

//     val r43 = Regex("(aA)++b")
//     "aAaAaAaAaAb" should matchPattern { case r43("aA") => }

//     val r44 = Regex("(aA)?+b")
//     "aAb" should matchPattern { case r44(Some("aA")) => }

//     val r45 = Regex("(aA){1,5}+b")
//     "aAaAaAaAaAb" should matchPattern { case r45("aA") => }

//     val r46 = Regex("(aA|bB)*+b")
//     "bBbBbBbBbBb" should matchPattern { case r46(Some("bB")) => }

//     val r47 = Regex("(aA|bB)++b")
//     "aAbBaAaAbBb" should matchPattern { case r47("bB") => }

//     val r48 = Regex("(aA|bB)?+b")
//     "bBb" should matchPattern { case r48(Some("bB")) => }

//     val r49 = Regex("(aA|bB){1,5}+b")
//     "bBaAbBaAbBb" should matchPattern { case r49("bB") => }

//     val r50 = Regex("(ab)?(ab)+")
//     "ab" should matchPattern { case r50(None, "ab") => }

//     val r51 = Regex("(abc)?(abc)+")
//     "abc" should matchPattern { case r51(None, "abc") => }

//     val r52 = Regex("(abc)\\1")
//     "abcabc" should matchPattern { case r52("abc") => }

//     val r53 = Regex("(ab|a)b*c")
//     "abc" should matchPattern { case r53("ab") => }

//     val r54 = Regex("(ab|ab*)bc")
//     "abc" should matchPattern { case r54("a") => }

//     // val r55 = Regex("(a|(bc)){0,0}+xyz")
//     // "xyz" should matchPattern { case r55(None, None) => }

//     // val r56 = Regex("(a|(bc)){0,0}?xyz")
//     // "xyz" should matchPattern { case r56(None, None) => }

//     val r57 = Regex("(a|b|c|d|e)f")
//     "ef" should matchPattern { case r57("e") => }

//     // TODO: No Idea what the type of this is meant to be
//     // val r58 = Regex("(bc+d$|ef*g.|h?i(j|k))")
//     // "effgz" should matchPattern { case r58("effgz", None) => }

//     // val r59 = Regex("(bc+d$|ef*g.|h?i(j|k))")
//     // "ij" should matchPattern { case r59(g0, Some(g1)) => assert((g0, g1) == ("ij", "j")) }

//     val r60 = Regex("(foo[1x]|bar[2x]|baz[3x])*y")
//     "foo1bar2baz3y" should matchPattern { case r60(Some("baz3")) => }

//     val r61 = Regex("(foo[1x]|bar[2x]|baz[3x])+y")
//     "foo1bar2baz3y" should matchPattern { case r61("baz3") => }

//     val r62 = Regex("(foo|fool|[zx].|money|parted)$")
//     "fool" should matchPattern { case r62("fool") => }

//     val r63 = Regex("(foo|fool|[zx].|money|parted)+$")
//     "fool" should matchPattern { case r63("fool") => }

//     val r64 = Regex("(foo|fool|money|parted)$")
//     "fool" should matchPattern { case r64("fool") => }

//     val r65 = Regex("(foo|fool|x.|money|parted)$")
//     "fool" should matchPattern { case r65("fool") => }

//     // TODO: There's a bug somewhere, should probably be `(Option[String], Option[String], String, Option[String])`

//     // val r66 = Regex("(q1|.)*(q2|.)*(x(a|bc)*y){2,3}")
//     // "xayxay" should matchPattern { case r66(None, None, "xay", Some("a")) => }

//     // val r67 = Regex("(q1|.)*(q2|.)*(x(a|bc)*y){2,}")
//     // "xayxay" should matchPattern { case r67(None, None, "xay", Some("a")) => }

//     // val r68 = Regex("(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z")
//     // "zzzzzzzzzzzzzzzz-xayxayxayxayZ" should matchPattern { case r68(Some("z"), None, "xay", Some("a")) => }

//     val r69 = Regex("(WORDS|WORD)S")
//     "WORDS" should matchPattern { case r69("WORD") => }

//     val r70 = Regex("(WORDS|WORLD|WORD)+S")
//     "WORDS" should matchPattern { case r70("WORD") => }

//     val r71 = Regex("(WORDS|WORLD|WORD)S")
//     "WORDS" should matchPattern { case r71("WORD") => }

//     val r72 = Regex("(x.|foo|fool|x.|money|parted|y.)$")
//     "fool" should matchPattern { case r72("fool") => }

//     val r73 = Regex("(X.|WORDS|WORD|Y.)S")
//     "WORDS" should matchPattern { case r73("WORD") => }

//     val r74 = Regex("(X.|WORDS|X.|WORD)S")
//     "WORDS" should matchPattern { case r74("WORD") => }

//     val r75 = Regex("(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*")
//     "xyzQzWlongishoverblownW" should matchPattern { case r75(Some("zW"), Some("overblownW")) => }

//     val r76 = Regex("(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+")
//     "xyzQzWlongishoverblownW" should matchPattern { case r76(Some("zW"), Some("overblownW")) => }

//     val r77 = Regex("(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+")
//     "xyzQzWlongishoverblownW" should matchPattern { case r77("zW", "overblownW") => }

//     val r78 = Regex("(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++")
//     "xyzQzWlongishoverblownW" should matchPattern { case r78("zW", "overblownW") => }

//     val r79 = Regex("(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}")
//     "xyzQzWlongishoverblownW" should matchPattern { case r79("zW", "overblownW") => }

//     val r80 = Regex("(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+")
//     "xyzQzWlongishoverblownW" should matchPattern { case r80("zW", "overblownW") => }

//     val r81 = Regex(".*?(?:(\\w)|(\\w))x")
//     "abx" should matchPattern { case r81(Left("b")) => }

//     val r82 = Regex("2(]*)?$\\1")
//     "2" should matchPattern { case r82(Some("")) => }

//     val r83 = Regex("\\((.*), (.*)\\)")
//     "(a, b)" should matchPattern { case r83("a", "b") => }

//     // TODO: No clue
//     // val r84 = Regex("^((?:aa)*)(?:X+((?:\\d+|-)(?:X+(.+))?))?$")
//     // "aaaaX5" should matchPattern { case r84("aaaa", Some(g1), None) => assert((g0, g1, null) == ("aaaa", "5", null)) }

//     val r85 = Regex("^((a|b)+)*ax")
//     "aax" should matchPattern { case r85(Some("a", "a")) => }

//     val r86 = Regex("^((a|bc)+)*ax")
//     "aax" should matchPattern { case r86(Some("a", "a")) => }

//     val r87 = Regex("^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$")
//     "text|\'sec\'" should matchPattern { case r87("text", "sec") => }

//     val r88 = Regex("^(.+)?B")
//     "AB" should matchPattern { case r88(Some("A")) => }

//     val r89 = Regex("^(.,){2}c")
//     "a,b,c" should matchPattern { case r89("b,") => }

//     val r90 = Regex("^(0+)?(?:x(1))?")
//     "x1" should matchPattern { case r90(None, Some("1")) => }

//     val r91 = Regex("^(?:(\\d)x)?\\d$")
//     "1" should matchPattern { case r91(None) => }

//     val r92 = Regex("^(?:(X)?(\\d)|(X)?(\\d\\d))$")
//     "X12" should matchPattern { case r92(Right(Some("X"), "12")) => }

//     val r93 = Regex("^(?:(XX)?(\\d)|(XX)?(\\d\\d))$")
//     "XX12" should matchPattern { case r93(Right(Some("XX"), "12")) => }

//     val r94 = Regex("^(?:f|o|b){2,3}?((?:b|a|r)+)\\1$")
//     "foobarbar" should matchPattern { case r94("bar") => }

//     val r95 = Regex("^(?:f|o|b){2,3}?((?:b|a|r)+?)\\1$")
//     "foobarbar" should matchPattern { case r95("bar") => }

//     val r96 = Regex("^(?:f|o|b){2,3}?(.+)\\1$")
//     "foobarbar" should matchPattern { case r96("bar") => }

//     val r97 = Regex("^(?:f|o|b){2,3}?(.+?)\\1$")
//     "foobarbar" should matchPattern { case r97("bar") => }

//     val r98 = Regex("^(?:f|o|b){3,4}((?:b|a|r)+)\\1$")
//     "foobarbar" should matchPattern { case r98("bar") => }

//     val r99 = Regex("^(?:f|o|b){3,4}((?:b|a|r)+?)\\1$")
//     "foobarbar" should matchPattern { case r99("bar") => }

//     val r100 = Regex("^(?:f|o|b){3,4}(.+)\\1$")
//     "foobarbar" should matchPattern { case r100("bar") => }

//     val r101 = Regex("^(?:f|o|b){3,4}(.+?)\\1$")
//     "foobarbar" should matchPattern { case r101("bar") => }

//     val r102 = Regex("^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?")
//     "012cxx0190" should matchPattern { case r102("012c", None, Some("0190")) => }

//     // val r103 = Regex("^([^,]*,){0,3}d")
//     // "aaa,b,c,d" should matchPattern { case r103(Some(g0)) => assert((g0) == ("c,")) }

//     val r104 = Regex("^([^,]*,){2}c")
//     "a,b,c" should matchPattern { case r104("b,") => }

//     val r105 = Regex("^([^,]*,){3,}d")
//     "aaa,b,c,d" should matchPattern { case r105("c,") => }

//     val r106 = Regex("^([^,]*,){3}d")
//     "aaa,b,c,d" should matchPattern { case r106("c,") => }

//     // val r107 = Regex("^([^,]{0,3},){0,3}d")
//     // "aaa,b,c,d" should matchPattern { case r107(Some(g0)) => assert((g0) == ("c,")) }

//     val r108 = Regex("^([^,]{0,3},){3,}d")
//     "aaa,b,c,d" should matchPattern { case r108("c,") => }

//     val r109 = Regex("^([^,]{0,3},){3}d")
//     "aaa,b,c,d" should matchPattern { case r109("c,") => }

//     // val r110 = Regex("^([^,]{1,3},){0,3}d")
//     // "aaa,b,c,d" should matchPattern { case r110(Some(g0)) => assert((g0) == ("c,")) }

//     val r111 = Regex("^([^,]{1,3},){3,}d")
//     "aaa,b,c,d" should matchPattern { case r111("c,") => }

//     val r112 = Regex("^([^,]{1,3},){3}d")
//     "aaa,b,c,d" should matchPattern { case r112("c,") => }

//     // val r113 = Regex("^([^,]{1,},){0,3}d")
//     // "aaa,b,c,d" should matchPattern { case r113(Some(g0)) => assert((g0) == ("c,")) }

//     val r114 = Regex("^([^,]{1,},){3,}d")
//     "aaa,b,c,d" should matchPattern { case r114("c,") => }

//     val r115 = Regex("^([^,]{1,},){3}d")
//     "aaa,b,c,d" should matchPattern { case r115("c,") => }

//     val r116 = Regex("^([^a-z])|(\\^)$")
//     "." should matchPattern { case r116(Left(".")) => }

//     val r117 = Regex("^([a]{1})*$")
//     "aa" should matchPattern { case r117(Some("a")) => }

//     val r118 = Regex("^([ab]*?)(b)?(c)$")
//     "abac" should matchPattern { case r118("aba", None, "c") => }

//     val r119 = Regex("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):")
//     "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r119("ZEQQQQQQQQQQQQQQQQQQP") => }

//     val r120 = Regex("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):")
//     "ZEQQQX:" should matchPattern { case r120("ZEQQQX") => }

//     val r121 = Regex("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):")
//     "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r121("ZEQQQQQQQQQQQQQQQQQQP") => }

//     val r122 = Regex("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):")
//     "ZEQQQX:" should matchPattern { case r122("ZEQQQX") => }

//     val r123 = Regex("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):")
//     "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r123("ZEQQQQQQQQQQQQQQQQQQP") => }

//     val r124 = Regex("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):")
//     "ZEQQQX:" should matchPattern { case r124("ZEQQQX") => }

//     val r125 = Regex("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):")
//     "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r125("ZEQQQQQQQQQQQQQQQQQQP") => }

//     val r126 = Regex("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):")
//     "ZEQQQX:" should matchPattern { case r126("ZEQQQX") => }

//     // TODO: Bug idk
//     // val r127 = Regex("^(a(b)?)+$")
//     // "aba" should matchPattern { case r127("a", Some("b")) => }

//     val r128 = Regex("^(a)?a$")
//     "a" should matchPattern { case r128(None) => }

//     val r129 = Regex("^(a+)*ax")
//     "aax" should matchPattern { case r129(Some("a")) => }

//     val r130 = Regex("^(a\\1?)(a\\1?)(a\\2?)(a\\3?)$")
//     "aaaaaa" should matchPattern { case r130("a", "aa", "a", "aa") => }

//     val r131 = Regex("^(a\\1?){4}$")
//     "aaaaaa" should matchPattern { case r131("aa") => }

//     val r132 = Regex("^(a\\1?){4}$")
//     "aaaaaaaaaa" should matchPattern { case r132("aaaa") => }

//     // TODO: Bug
//     // val r133 = Regex("^(aa(bb)?)+$")
//     // "aabbaa" should matchPattern { case r133(g0, Some(g1)) => assert((g0, g1) == ("aa", "bb")) }

//     val r134 = Regex("^(b+?|a){1,2}c")
//     "bbbac" should matchPattern { case r134("a") => }

//     val r135 = Regex("^(b+?|a){1,2}c")
//     "bbbbac" should matchPattern { case r135("a") => }

//     val r136 = Regex("^(foo|)bar$")
//     "bar" should matchPattern { case r136("") => }

//     val r137 = Regex("^(foo||baz)bar$")
//     "bar" should matchPattern { case r137("") => }

//     val r138 = Regex("^(foo||baz)bar$")
//     "bazbar" should matchPattern { case r138("baz") => }

//     val r139 = Regex("^(foo||baz)bar$")
//     "foobar" should matchPattern { case r139("foo") => }

//     val r140 = Regex("^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):")
//     "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r140("ZEQQQQQQQQQQQQQQQQQQP") => }

//     val r141 = Regex("^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):")
//     "ZEQQQX:" should matchPattern { case r141("ZEQQQX") => }

//     val r142 = Regex("^(XXX|YYY|Z.Q*X|Z[TE]Q*P):")
//     "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r142("ZEQQQQQQQQQQQQQQQQQQP") => }

//     val r143 = Regex("^(XXX|YYY|Z.Q*X|Z[TE]Q*P):")
//     "ZEQQQX:" should matchPattern { case r143("ZEQQQX") => }

//     val r144 = Regex("^.{2,3}?((?:b|a|r)+)\\1$")
//     "foobarbar" should matchPattern { case r144("bar") => }

//     val r145 = Regex("^.{2,3}?((?:b|a|r)+?)\\1$")
//     "foobarbar" should matchPattern { case r145("bar") => }

//     val r146 = Regex("^.{2,3}?(.+)\\1$")
//     "foobarbar" should matchPattern { case r146("bar") => }

//     val r147 = Regex("^.{2,3}?(.+?)\\1$")
//     "foobarbar" should matchPattern { case r147("bar") => }

//     val r148 = Regex("^.{3,4}((?:b|a|r)+)\\1$")
//     "foobarbar" should matchPattern { case r148("bar") => }

//     val r149 = Regex("^.{3,4}((?:b|a|r)+?)\\1$")
//     "foobarbar" should matchPattern { case r149("bar") => }

//     val r150 = Regex("^.{3,4}(.+)\\1$")
//     "foobarbar" should matchPattern { case r150("bar") => }

//     val r151 = Regex("^.{3,4}(.+?)\\1$")
//     "foobarbar" should matchPattern { case r151("bar") => }

//     val r152 = Regex("^m?(\\d)(.*)\\1$")
//     "5b5" should matchPattern { case r152("5", "b") => }

//     val r153 = Regex("^m?(\\D)(.*)\\1$")
//     "aba" should matchPattern { case r153("a", "b") => }

//     val r154 = Regex("^m?(\\S)(.*)\\1$")
//     "aba" should matchPattern { case r154("a", "b") => }

//     val r155 = Regex("^m?(\\W)(.*)\\1$")
//     ":b:" should matchPattern { case r155(":", "b") => }

//     val r156 = Regex("^m?(\\w)(.*)\\1$")
//     "aba" should matchPattern { case r156("a", "b") => }

//     // TODO: No clue what type
//     // val r157 = Regex("a(?:b|(c|e){1,2}?|d)+?(.)")
//     // "ace" should matchPattern { case r157(Some(g0), g1) => assert((g0, g1) == ("c", "e")) }

//     val r158 = Regex("a(?:b|c|d)(.)")
//     "ace" should matchPattern { case r158("e") => }

//     val r159 = Regex("a(?:b|c|d)*(.)")
//     "ace" should matchPattern { case r159("e") => }

//     val r160 = Regex("a(?:b|c|d)+(.)")
//     "acdbcdbe" should matchPattern { case r160("e") => }

//     val r161 = Regex("a(?:b|c|d)+?(.)")
//     "acdbcdbe" should matchPattern { case r161("e") => }

//     val r162 = Regex("a(?:b|c|d)+?(.)")
//     "ace" should matchPattern { case r162("e") => }

//     val r163 = Regex("a(?:b|c|d){5,6}(.)")
//     "acdbcdbe" should matchPattern { case r163("e") => }

//     val r164 = Regex("a(?:b|c|d){5,6}?(.)")
//     "acdbcdbe" should matchPattern { case r164("e") => }

//     val r165 = Regex("a(?:b|c|d){5,7}(.)")
//     "acdbcdbe" should matchPattern { case r165("e") => }

//     val r166 = Regex("a(?:b|c|d){5,7}?(.)")
//     "acdbcdbe" should matchPattern { case r166("e") => }

//     val r167 = Regex("a(?:b|c|d){6,7}(.)")
//     "acdbcdbe" should matchPattern { case r167("e") => }

//     val r168 = Regex("a(?:b|c|d){6,7}?(.)")
//     "acdbcdbe" should matchPattern { case r168("e") => }

//     val r169 = Regex("a([bc]*)(c*d)")
//     "abcd" should matchPattern { case r169("bc", "d") => }

//     val r170 = Regex("a([bc]*)(c+d)")
//     "abcd" should matchPattern { case r170("b", "cd") => }

//     val r171 = Regex("a([bc]*)c*")
//     "abc" should matchPattern { case r171("bc") => }

//     val r172 = Regex("a([bc]+)(c*d)")
//     "abcd" should matchPattern { case r172("bc", "d") => }

//     val r173 = Regex("a(bc)d")
//     "abcd" should matchPattern { case r173("bc") => }

//     val r174 = Regex("foo(aA)*+b")
//     "fooaAaAaAaAaAb" should matchPattern { case r174(Some("aA")) => }

//     val r175 = Regex("foo(aA)++b")
//     "fooaAaAaAaAaAb" should matchPattern { case r175("aA") => }

//     val r176 = Regex("foo(aA)?+b")
//     "fooaAb" should matchPattern { case r176(Some("aA")) => }

//     val r177 = Regex("foo(aA){1,5}+b")
//     "fooaAaAaAaAaAb" should matchPattern { case r177("aA") => }

//     val r178 = Regex("foo(aA|bB)*+b")
//     "foobBbBaAaAaAb" should matchPattern { case r178(Some("aA")) => }

//     val r179 = Regex("foo(aA|bB)++b")
//     "foobBaAbBaAbBb" should matchPattern { case r179("bB") => }

//     val r180 = Regex("foo(aA|bB)?+b")
//     "foobBb" should matchPattern { case r180(Some("bB")) => }

//     val r181 = Regex("foo(aA|bB){1,5}+b")
//     "foobBaAaAaAaAb" should matchPattern { case r181("aA") => }

//     val r182 = Regex("X(\\w+)(?=\\s)|X(\\w+)")
//     "Xab" should matchPattern { case r182(Right("ab")) => }

//     val r183 = Regex("x(~~)*(?:(?:F)?)?")
//     "x~~" should matchPattern { case r183(Some("~~")) => }
//   }
// }
