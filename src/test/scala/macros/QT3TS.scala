package experiments.macros

import experiments.macros.catnip.r
import cats.data.Ior.Both
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class QT3TSTests extends AnyFlatSpec {
  behavior of "Regex macro"

  it should "pass test 1: ((((((((((a))))))))))" in {
    val r1 = r"((((((((((a))))))))))"
    "a" should matchPattern { case r1("a", "a", "a", "a", "a", "a", "a", "a", "a", "a") => }
  }

  it should "pass test 2: ((((((((((a))))))))))\\10" in {
    val r2 = r"((((((((((a))))))))))\10"
    "aa" should matchPattern { case r2("a", "a", "a", "a", "a", "a", "a", "a", "a", "a") => }
  }

  it should "pass test 3: (((((((((a)))))))))" in {
    val r3 = r"(((((((((a)))))))))"
    "a" should matchPattern { case r3("a", "a", "a", "a", "a", "a", "a", "a", "a") => }
  }

  it should "pass test 4: ((?:aaaa|bbbb)cccc)?" in {
    val r4 = r"((?:aaaa|bbbb)cccc)?"
    "aaaacccc" should matchPattern { case r4(Some("aaaacccc")) => }
  }

  it should "pass test 5: ((?:aaaa|bbbb)cccc)?" in {
    val r5 = r"((?:aaaa|bbbb)cccc)?"
    "bbbbcccc" should matchPattern { case r5(Some("bbbbcccc")) => }
  }

  it should "pass test 6: ((?i)a)b" in {
    val r6 = r"((?i)a)b"
    "ab" should matchPattern { case r6("a") => }
  }

  it should "pass test 7: ((?i)a)b" in {
    val r7 = r"((?i)a)b"
    "Ab" should matchPattern { case r7("A") => }
  }

  it should "pass test 8: ((?i:a))b" in {
    val r8 = r"((?i:a))b"
    "ab" should matchPattern { case r8("a") => }
  }

  it should "pass test 9: ((?i:a))b" in {
    val r9 = r"((?i:a))b"
    "Ab" should matchPattern { case r9("A") => }
  }

  it should "pass test 10: (([a-c])b*?\\2)*" in {
    val r10 = r"(([a-c])b*?\2)*"
    "ababbbcbc" should matchPattern { case r10(Some("cbc", "c")) => }
  }

  it should "pass test 11: (([a-c])b*?\\2){3}" in {
    val r11 = r"(([a-c])b*?\2){3}"
    "ababbbcbc" should matchPattern { case r11("cbc", "c") => }
  }

  it should "pass test 12: ((a)(b)c)(d)" in {
    val r12 = r"((a)(b)c)(d)"
    "abcd" should matchPattern { case r12("abc", "a", "b", "d") => }
  }

  it should "pass test 13: ((foo)|(bar))*" in {
    val r13 = r"((foo)|(bar))*"
    "foobar" should matchPattern { case r13(Some("bar", Both("foo", "bar"))) => }
  }

  it should "pass test 14: (.*)c(.*)" in {
    val r14 = r"(.*)c(.*)"
    "abcde" should matchPattern { case r14("ab", "de") => }
  }

  it should "pass test 15: (?:(f)(o)(o)|(b)(a)(r))*" in {
    val r15 = r"(?:(f)(o)(o)|(b)(a)(r))*"
    "foobar" should matchPattern { case r15(Some(Both(("f", "o", "o"), ("b", "a", "r")))) => }
  }

  it should "pass test 16: ([[:digit:]-[:alpha:]]+)" in pending // {
  //   val r16 = r"([[:digit:]-[:alpha:]]+)"
  //   "-" should matchPattern { case r16("-") => }
  // }

  it should "pass test 17: ([[:digit:]-z]+)" in pending // {
  //   val r17 = r"([[:digit:]-z]+)"
  //   "-" should matchPattern { case r17("-") => }
  // }

  // TODO: Parser bug
  it should "pass test 18: ([\\d-\\s]+)" in pending // {
  //   val r18 = r"([\\d-\\s]+)"
  //   "-" should matchPattern { case r18("-") => }
  // }

  // TODO: Parser bug
  it should "pass test 19: ([\\d-z]+)" in pending // {
  //   val r19 = r"([\\d-z]+)"
  //   "-" should matchPattern { case r19("-") => }
  // }

  // TODO: Parser bug
  it should "pass test 20: ([\\w:]+::)?(\\w+)$" in pending // {
  //   val r20 = r"([\w:]+::)?(\w+)$$"
  //   "abcd" should matchPattern { case r20(None, "abcd") => }
  // }

  // TODO: Parser bug
  it should "pass test 21: ([\\w:]+::)?(\\w+)$" in pending // {
  //   val r21 = r"([\w:]+::)?(\w+)$$"
  //   "xy:z:::abcd" should matchPattern { case r21(Some("xy:z:::"), "abcd") => }
  // }

  it should "pass test 22: ([a-c]*)\\1" in {
    val r22 = r"([a-c]*)\1"
    "abcabc" should matchPattern { case r22("abc") => }
  }

  it should "pass test 23: ([abc])*bcd" in {
    val r23 = r"([abc])*bcd"
    "abcd" should matchPattern { case r23(Some("a")) => }
  }

  it should "pass test 24: ([abc])*d" in {
    val r24 = r"([abc])*d"
    "abbbcd" should matchPattern { case r24(Some("c")) => }
  }

  it should "pass test 25: ([yX].|WORDS|[yX].|WORD)+S" in {
    val r25 = r"([yX].|WORDS|[yX].|WORD)+S"
    "WORDS" should matchPattern { case r25("WORD") => }
  }

  it should "pass test 26: ([yX].|WORDS|[yX].|WORD)S" in {
    val r26 = r"([yX].|WORDS|[yX].|WORD)S"
    "WORDS" should matchPattern { case r26("WORD") => }
  }

  it should "pass test 27: ([yX].|WORDS|WORD|[xY].)+S" in {
    val r27 = r"([yX].|WORDS|WORD|[xY].)+S"
    "WORDS" should matchPattern { case r27("WORD") => }
  }

  it should "pass test 28: ([yX].|WORDS|WORD|[xY].)S" in {
    val r28 = r"([yX].|WORDS|WORD|[xY].)S"
    "WORDS" should matchPattern { case r28("WORD") => }
  }

  it should "pass test 29: ([zx].|foo|fool|[zq].|money|parted|[yx].)$" in {
    val r29 = r"([zx].|foo|fool|[zq].|money|parted|[yx].)$$"
    "fool" should matchPattern { case r29("fool") => }
  }

  it should "pass test 30: ([zx].|foo|fool|[zq].|money|parted|[yx].)+$" in {
    val r30 = r"([zx].|foo|fool|[zq].|money|parted|[yx].)+$$"
    "fool" should matchPattern { case r30("fool") => }
  }

  it should "pass test 31: (\\d+\\.\\d+)" in {
    val r31 = r"(\d+\.\d+)"
    "3.1415926" should matchPattern { case r31("3.1415926") => }
  }

  it should "pass test 32: (\\w+:)+" in {
    val r32 = r"(\w+:)+"
    "one:" should matchPattern { case r32("one:") => }
  }

  it should "pass test 33: (^|a)b" in {
    val r33 = r"(^|a)b"
    "ab" should matchPattern { case r33("a") => }
  }

  it should "pass test 34: (a)?(a)+" in {
    val r34 = r"(a)?(a)+"
    "a" should matchPattern { case r34(None, "a") => }
  }

  it should "pass test 35: (a)b(c)" in {
    val r35 = r"(a)b(c)"
    "abc" should matchPattern { case r35("a", "c") => }
  }

  it should "pass test 36: (a)|(b)" in {
    val r36 = r"(a)|(b)"
    "b" should matchPattern { case r36(Right("b")) => }
  }

  it should "pass test 37: (a)|\\1" in {
    val r37 = r"(a)|\1"
    "a" should matchPattern { case r37(Some("a")) => }
  }

  it should "pass test 38: (a+|b)*" in {
    val r38 = r"(a+|b)*"
    "ab" should matchPattern { case r38(Some("b")) => }
  }

  it should "pass test 39: (a+|b)+" in {
    val r39 = r"(a+|b)+"
    "ab" should matchPattern { case r39("b") => }
  }

  it should "pass test 40: (a+|b){0,}" in {
    val r40 = r"(a+|b){0,}"
    "ab" should matchPattern { case r40(Some("b")) => }
  }

  it should "pass test 41: (a+|b){1,}" in {
    val r41 = r"(a+|b){1,}"
    "ab" should matchPattern { case r41("b") => }
  }

  it should "pass test 42: (aA)*+b" in {
    val r42 = r"(aA)*+b"
    "aAaAaAaAaAb" should matchPattern { case r42(Some("aA")) => }
  }

  it should "pass test 43: (aA)++b" in {
    val r43 = r"(aA)++b"
    "aAaAaAaAaAb" should matchPattern { case r43("aA") => }
  }

  it should "pass test 44: (aA)?+b" in {
    val r44 = r"(aA)?+b"
    "aAb" should matchPattern { case r44(Some("aA")) => }
  }

  it should "pass test 45: (aA){1,5}+b" in {
    val r45 = r"(aA){1,5}+b"
    "aAaAaAaAaAb" should matchPattern { case r45("aA") => }
  }

  it should "pass test 46: (aA|bB)*+b" in {
    val r46 = r"(aA|bB)*+b"
    "bBbBbBbBbBb" should matchPattern { case r46(Some("bB")) => }
  }

  it should "pass test 47: (aA|bB)++b" in {
    val r47 = r"(aA|bB)++b"
    "aAbBaAaAbBb" should matchPattern { case r47("bB") => }
  }

  it should "pass test 48: (aA|bB)?+b" in {
    val r48 = r"(aA|bB)?+b"
    "bBb" should matchPattern { case r48(Some("bB")) => }
  }

  it should "pass test 49: (aA|bB){1,5}+b" in {
    val r49 = r"(aA|bB){1,5}+b"
    "bBaAbBaAbBb" should matchPattern { case r49("bB") => }
  }

  it should "pass test 50: (ab)?(ab)+" in {
    val r50 = r"(ab)?(ab)+"
    "ab" should matchPattern { case r50(None, "ab") => }
  }

  it should "pass test 51: (abc)?(abc)+" in {
    val r51 = r"(abc)?(abc)+"
    "abc" should matchPattern { case r51(None, "abc") => }
  }

  it should "pass test 52: (abc)\\1" in {
    val r52 = r"(abc)\1"
    "abcabc" should matchPattern { case r52("abc") => }
  }

  it should "pass test 53: (ab|a)b*c" in {
    val r53 = r"(ab|a)b*c"
    "abc" should matchPattern { case r53("ab") => }
  }

  it should "pass test 54: (ab|ab*)bc" in {
    val r54 = r"(ab|ab*)bc"
    "abc" should matchPattern { case r54("a") => }
  }

  it should "pass test 55: (a|(bc)){0,0}+xyz" in {
    val r55 = r"(a|(bc)){0,0}+xyz"
    "xyz" should matchPattern { case r55(()) => }
  }

  it should "pass test 56: (a|(bc)){0,0}?xyz" in {
    val r56 = r"(a|(bc)){0,0}?xyz"
    "xyz" should matchPattern { case r56(()) => }
  }

  it should "pass test 57: (a|b|c|d|e)f" in {
    val r57 = r"(a|b|c|d|e)f"
    "ef" should matchPattern { case r57("e") => }
  }

  it should "pass test 58: (bc+d$|ef*g.|h?i(j|k))" in {
    val r58 = r"(bc+d$$|ef*g.|h?i(j|k))"
    "effgz" should matchPattern { case r58("effgz", _) => }
  }

  it should "pass test 59: (bc+d$|ef*g.|h?i(j|k))" in {
    val r59 = r"(bc+d$$|ef*g.|h?i(j|k))"
    "ij" should matchPattern { case r59("ij", Some(Some("j"))) => }
  }

  it should "pass test 60: (foo[1x]|bar[2x]|baz[3x])*y" in {
    val r60 = r"(foo[1x]|bar[2x]|baz[3x])*y"
    "foo1bar2baz3y" should matchPattern { case r60(Some("baz3")) => }
  }

  it should "pass test 61: (foo[1x]|bar[2x]|baz[3x])+y" in {
    val r61 = r"(foo[1x]|bar[2x]|baz[3x])+y"
    "foo1bar2baz3y" should matchPattern { case r61("baz3") => }
  }

  it should "pass test 62: (foo|fool|[zx].|money|parted)$" in {
    val r62 = r"(foo|fool|[zx].|money|parted)$$"
    "fool" should matchPattern { case r62("fool") => }
  }

  it should "pass test 63: (foo|fool|[zx].|money|parted)+$" in {
    val r63 = r"(foo|fool|[zx].|money|parted)+$$"
    "fool" should matchPattern { case r63("fool") => }
  }

  it should "pass test 64: (foo|fool|money|parted)$" in {
    val r64 = r"(foo|fool|money|parted)$$"
    "fool" should matchPattern { case r64("fool") => }
  }

  it should "pass test 65: (foo|fool|x.|money|parted)$" in {
    val r65 = r"(foo|fool|x.|money|parted)$$"
    "fool" should matchPattern { case r65("fool") => }
  }

  it should "pass test 66: (q1|.)*(q2|.)*(x(a|bc)*y){2,3}" in {
    val r66 = r"(q1|.)*(q2|.)*(x(a|bc)*y){2,3}"
    "xayxay" should matchPattern { case r66(None, None, "xay", Some("a")) => }
  }

  it should "pass test 67: (q1|.)*(q2|.)*(x(a|bc)*y){2,}" in {
    val r67 = r"(q1|.)*(q2|.)*(x(a|bc)*y){2,}"
    "xayxay" should matchPattern { case r67(None, None, "xay", Some("a")) => }
  }

  it should "pass test 68: (q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z" in {
    val r68 = r"(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z"
    "zzzzzzzzzzzzzzzz-xayxayxayxayZ" should matchPattern { case r68(Some("z"), None, "xay", Some("a")) => }
  }

  it should "pass test 69: (WORDS|WORD)S" in {
    val r69 = r"(WORDS|WORD)S"
    "WORDS" should matchPattern { case r69("WORD") => }
  }

  it should "pass test 70: (WORDS|WORLD|WORD)+S" in {
    val r70 = r"(WORDS|WORLD|WORD)+S"
    "WORDS" should matchPattern { case r70("WORD") => }
  }

  it should "pass test 71: (WORDS|WORLD|WORD)S" in {
    val r71 = r"(WORDS|WORLD|WORD)S"
    "WORDS" should matchPattern { case r71("WORD") => }
  }

  it should "pass test 72: (x.|foo|fool|x.|money|parted|y.)$" in {
    val r72 = r"(x.|foo|fool|x.|money|parted|y.)$$"
    "fool" should matchPattern { case r72("fool") => }
  }

  it should "pass test 73: (X.|WORDS|WORD|Y.)S" in {
    val r73 = r"(X.|WORDS|WORD|Y.)S"
    "WORDS" should matchPattern { case r73("WORD") => }
  }

  it should "pass test 74: (X.|WORDS|X.|WORD)S" in {
    val r74 = r"(X.|WORDS|X.|WORD)S"
    "WORDS" should matchPattern { case r74("WORD") => }
  }

  it should "pass test 75: (x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*" in {
    val r75 = r"(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*"
    "xyzQzWlongishoverblownW" should matchPattern { case r75(Some("zW"), Some("overblownW")) => }
  }

  it should "pass test 76: (x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+" in {
    val r76 = r"(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+"
    "xyzQzWlongishoverblownW" should matchPattern { case r76(Some("zW"), Some("overblownW")) => }
  }

  it should "pass test 77: (x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+" in {
    val r77 = r"(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+"
    "xyzQzWlongishoverblownW" should matchPattern { case r77("zW", "overblownW") => }
  }

  it should "pass test 78: (x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++" in {
    val r78 = r"(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++"
    "xyzQzWlongishoverblownW" should matchPattern { case r78("zW", "overblownW") => }
  }

  it should "pass test 79: (x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}" in {
    val r79 = r"(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}"
    "xyzQzWlongishoverblownW" should matchPattern { case r79("zW", "overblownW") => }
  }

  it should "pass test 80: (x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+" in {
    val r80 = r"(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+"
    "xyzQzWlongishoverblownW" should matchPattern { case r80("zW", "overblownW") => }
  }

  it should "pass test 81: .*?(?:(\\w)|(\\w))x" in {
    val r81 = r".*?(?:(\w)|(\w))x"
    "abx" should matchPattern { case r81(Left("b")) => }
  }

  it should "pass test 82: 2(]*)?$\\1" in {
    val r82 = r"2(]*)?$$\1"
    "2" should matchPattern { case r82(Some("")) => }
  }

  it should "pass test 83: \\((.*), (.*)\\)" in {
    val r83 = r"\((.*), (.*)\)"
    "(a, b)" should matchPattern { case r83("a", "b") => }
  }

  it should "pass test 84: ^((?:aa)*)(?:X+((?:\\d+|-)(?:X+(.+))?))?$" in {
    val r84 = r"^((?:aa)*)(?:X+((?:\d+|-)(?:X+(.+))?))?$$"
    "aaaaX5" should matchPattern { case r84("aaaa", Some("5", None)) => }
  }

  it should "pass test 85: ^((a|b)+)*ax" in {
    val r85 = r"^((a|b)+)*ax"
    "aax" should matchPattern { case r85(Some("a", "a")) => }
  }

  it should "pass test 86: ^((a|bc)+)*ax" in {
    val r86 = r"^((a|bc)+)*ax"
    "aax" should matchPattern { case r86(Some("a", "a")) => }
  }

  it should "pass test 87: ^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$" in pending // {
  //   val r87 = r"^(.*?)\s*\|\s*(?:\/\s*|)'(.+)'$$"
  //   "text|\'sec\'" should matchPattern { case r87("text", "sec") => }
  // }

  it should "pass test 88: ^(.+)?B" in {
    val r88 = r"^(.+)?B"
    "AB" should matchPattern { case r88(Some("A")) => }
  }

  it should "pass test 89: ^(.,){2}c" in {
    val r89 = r"^(.,){2}c"
    "a,b,c" should matchPattern { case r89("b,") => }
  }

  it should "pass test 90: ^(0+)?(?:x(1))?" in {
    val r90 = r"^(0+)?(?:x(1))?"
    "x1" should matchPattern { case r90(None, Some("1")) => }
  }

  it should "pass test 91: ^(?:(\\d)x)?\\d$" in {
    val r91 = r"^(?:(\d)x)?\d$$"
    "1" should matchPattern { case r91(None) =>}
  }

  it should "pass test 92: ^(?:(X)?(\\d)|(X)?(\\d\\d))$" in {
    val r92 = r"^(?:(X)?(\d)|(X)?(\d\d))$$"
    "X12" should matchPattern { case r92(Right(Some("X"), "12")) => }
  }

  it should "pass test 93: ^(?:(XX)?(\\d)|(XX)?(\\d\\d))$" in {
    val r93 = r"^(?:(XX)?(\d)|(XX)?(\d\d))$$"
    "XX12" should matchPattern { case r93(Right(Some("XX"), "12")) => }
  }

  it should "pass test 94: ^(?:f|o|b){2,3}?((?:b|a|r)+)\\1$" in {
    val r94 = r"^(?:f|o|b){2,3}?((?:b|a|r)+)\1$$"
    "foobarbar" should matchPattern { case r94("bar") => }
  }

  it should "pass test 95: ^(?:f|o|b){2,3}?((?:b|a|r)+?)\\1$" in {
    val r95 = r"^(?:f|o|b){2,3}?((?:b|a|r)+?)\1$$"
    "foobarbar" should matchPattern { case r95("bar") => }
  }

  it should "pass test 96: ^(?:f|o|b){2,3}?(.+)\\1$" in {
    val r96 = r"^(?:f|o|b){2,3}?(.+)\1$$"
    "foobarbar" should matchPattern { case r96("bar") => }
  }

  it should "pass test 97: ^(?:f|o|b){2,3}?(.+?)\\1$" in {
    val r97 = r"^(?:f|o|b){2,3}?(.+?)\1$$"
    "foobarbar" should matchPattern { case r97("bar") => }
  }

  it should "pass test 98: ^(?:f|o|b){3,4}((?:b|a|r)+)\\1$" in {
    val r98 = r"^(?:f|o|b){3,4}((?:b|a|r)+)\1$$"
    "foobarbar" should matchPattern { case r98("bar") => }
  }

  it should "pass test 99: ^(?:f|o|b){3,4}((?:b|a|r)+?)\\1$" in {
    val r99 = r"^(?:f|o|b){3,4}((?:b|a|r)+?)\1$$"
    "foobarbar" should matchPattern { case r99("bar") => }
  }

  it should "pass test 100: ^(?:f|o|b){3,4}(.+)\\1$" in {
    val r100 = r"^(?:f|o|b){3,4}(.+)\1$$"
    "foobarbar" should matchPattern { case r100("bar") => }
  }

  it should "pass test 101: ^(?:f|o|b){3,4}(.+?)\\1$" in {
    val r101 = r"^(?:f|o|b){3,4}(.+?)\1$$"
    "foobarbar" should matchPattern { case r101("bar") => }
  }

  it should "pass test 102: ^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?" in {
    val r102 = r"^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?"
    "012cxx0190" should matchPattern { case r102("012c", None, Some("0190")) => }
  }

  it should "pass test 103: ^([^,]*,){0,3}d" in {
    val r103 = r"^([^,]*,){0,3}d"
    "aaa,b,c,d" should matchPattern { case r103(Some("c,")) => }
  }

  it should "pass test 104: ^([^,]*,){2}c" in {
    val r104 = r"^([^,]*,){2}c"
    "a,b,c" should matchPattern { case r104("b,") => }
  }

  it should "pass test 105: ^([^,]*,){3,}d" in {
    val r105 = r"^([^,]*,){3,}d"
    "aaa,b,c,d" should matchPattern { case r105("c,") => }
  }

  it should "pass test 106: ^([^,]*,){3}d" in {
    val r106 = r"^([^,]*,){3}d"
    "aaa,b,c,d" should matchPattern { case r106("c,") => }
  }

  it should "pass test 107: ^([^,]{0,3},){0,3}d" in {
    val r107 = r"^([^,]{0,3},){0,3}d"
    "aaa,b,c,d" should matchPattern { case r107(Some("c,")) => }
  }

  it should "pass test 108: ^([^,]{0,3},){3,}d" in {
    val r108 = r"^([^,]{0,3},){3,}d"
    "aaa,b,c,d" should matchPattern { case r108("c,") => }
  }

  it should "pass test 109: ^([^,]{0,3},){3}d" in {
    val r109 = r"^([^,]{0,3},){3}d"
    "aaa,b,c,d" should matchPattern { case r109("c,") => }
  }

  it should "pass test 110: ^([^,]{1,3},){0,3}d" in {
    val r110 = r"^([^,]{1,3},){0,3}d"
    "aaa,b,c,d" should matchPattern { case r110(Some("c,")) => }
  }

  it should "pass test 111: ^([^,]{1,3},){3,}d" in {
    val r111 = r"^([^,]{1,3},){3,}d"
    "aaa,b,c,d" should matchPattern { case r111("c,") => }
  }

  it should "pass test 112: ^([^,]{1,3},){3}d" in {
    val r112 = r"^([^,]{1,3},){3}d"
    "aaa,b,c,d" should matchPattern { case r112("c,") => }
  }

  it should "pass test 113: ^([^,]{1,},){0,3}d" in {
    val r113 = r"^([^,]{1,},){0,3}d"
    "aaa,b,c,d" should matchPattern { case r113(Some("c,")) => }
  }

  it should "pass test 114: ^([^,]{1,},){3,}d" in {
    val r114 = r"^([^,]{1,},){3,}d"
    "aaa,b,c,d" should matchPattern { case r114("c,") => }
  }

  it should "pass test 115: ^([^,]{1,},){3}d" in {
    val r115 = r"^([^,]{1,},){3}d"
    "aaa,b,c,d" should matchPattern { case r115("c,") => }
  }

  it should "pass test 116: ^([^a-z])|(\\^)$" in {
    val r116 = r"^([^a-z])|(\^)$$"
    "." should matchPattern { case r116(Left(".")) => }
  }

  it should "pass test 117: ^([a]{1})*$" in {
    val r117 = r"^([a]{1})*$$"
    "aa" should matchPattern { case r117(Some("a")) => }
  }

  it should "pass test 118: ^([ab]*?)(b)?(c)$" in {
    val r118 = r"^([ab]*?)(b)?(c)$$"
    "abac" should matchPattern { case r118("aba", None, "c") => }
  }

  it should "pass test 119: ^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):" in {
    val r119 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
    "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r119("ZEQQQQQQQQQQQQQQQQQQP") => }
  }

  it should "pass test 120: ^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):" in {
    val r120 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
    "ZEQQQX:" should matchPattern { case r120("ZEQQQX") => }
  }

  it should "pass test 121: ^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):" in {
    val r121 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
    "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r121("ZEQQQQQQQQQQQQQQQQQQP") => }
  }

  it should "pass test 122: ^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):" in {
    val r122 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
    "ZEQQQX:" should matchPattern { case r122("ZEQQQX") => }
  }

  it should "pass test 123: ^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):" in {
    val r123 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"
    "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r123("ZEQQQQQQQQQQQQQQQQQQP") => }
  }

  it should "pass test 124: ^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):" in {
    val r124 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"
    "ZEQQQX:" should matchPattern { case r124("ZEQQQX") => }
  }

  it should "pass test 125: ^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):" in {
    val r125 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
    "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r125("ZEQQQQQQQQQQQQQQQQQQP") => }
  }

  it should "pass test 126: ^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):" in {
    val r126 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
    "ZEQQQX:" should matchPattern { case r126("ZEQQQX") => }
  }

  it should "pass test 127: ^(a(b)?)+$" in {
    val r127 = r"^(a(b)?)+$$"
    "aba" should matchPattern { case r127("a", Some("b")) => }
  }

  it should "pass test 128: ^(a)?a$" in {
    val r128 = r"^(a)?a$$"
    "a" should matchPattern { case r128(None) => }
  }

  it should "pass test 129: ^(a+)*ax" in {
    val r129 = r"^(a+)*ax"
    "aax" should matchPattern { case r129(Some("a")) => }
  }

  it should "pass test 130: ^(a\\1?)(a\\1?)(a\\2?)(a\\3?)$" in {
    val r130 = r"^(a\1?)(a\1?)(a\2?)(a\3?)$$"
    "aaaaaa" should matchPattern { case r130("a", "aa", "a", "aa") => }
  }

  it should "pass test 131: ^(a\\1?){4}$" in {
    val r131 = r"^(a\1?){4}$$"
    "aaaaaa" should matchPattern { case r131("aa") => }
  }

  it should "pass test 132: ^(a\\1?){4}$" in {
    val r132 = r"^(a\1?){4}$$"
    "aaaaaaaaaa" should matchPattern { case r132("aaaa") => }
  }

  it should "pass test 133: ^(aa(bb)?)+$" in {
    val r133 = r"^(aa(bb)?)+$$"
    "aabbaa" should matchPattern { case r133("aa", Some("bb")) => }
  }

  it should "pass test 134: ^(b+?|a){1,2}c" in {
    val r134 = r"^(b+?|a){1,2}c"
    "bbbac" should matchPattern { case r134("a") => }
  }

  it should "pass test 135: ^(b+?|a){1,2}c" in {
    val r135 = r"^(b+?|a){1,2}c"
    "bbbbac" should matchPattern { case r135("a") => }
  }

  it should "pass test 136: ^(foo|)bar$" in pending // {
  //   val r136 = r"^(foo|)bar$"
  //   "bar" should matchPattern { case r136("") => }
  // }

  it should "pass test 137: ^(foo||baz)bar$" in pending // {
  //   val r137 = r"^(foo||baz)bar$"
  //   "bar" should matchPattern { case r137("") => }
  // }

  it should "pass test 138: ^(foo||baz)bar$" in pending // {
  //   val r138 = r"^(foo||baz)bar$"
  //   "bazbar" should matchPattern { case r138("baz") => }
  // }

  it should "pass test 139: ^(foo||baz)bar$" in pending // {
  //   val r139 = r"^(foo||baz)bar$"
  //   "foobar" should matchPattern { case r139("foo") => }
  // }

  it should "pass test 140: ^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):" in {
    val r140 = r"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
    "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r140("ZEQQQQQQQQQQQQQQQQQQP") => }
  }

  it should "pass test 141: ^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):" in {
    val r141 = r"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
    "ZEQQQX:" should matchPattern { case r141("ZEQQQX") => }
  }

  it should "pass test 142: ^(XXX|YYY|Z.Q*X|Z[TE]Q*P):" in {
    val r142 = r"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"
    "ZEQQQQQQQQQQQQQQQQQQP:" should matchPattern { case r142("ZEQQQQQQQQQQQQQQQQQQP") => }
  }

  it should "pass test 143: ^(XXX|YYY|Z.Q*X|Z[TE]Q*P):" in {
    val r143 = r"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"
    "ZEQQQX:" should matchPattern { case r143("ZEQQQX") => }
  }

  it should "pass test 144: ^.{2,3}?((?:b|a|r)+)\\1$" in {
    val r144 = r"^.{2,3}?((?:b|a|r)+)\1$$"
    "foobarbar" should matchPattern { case r144("bar") => }
  }

  it should "pass test 145: ^.{2,3}?((?:b|a|r)+?)\\1$" in {
    val r145 = r"^.{2,3}?((?:b|a|r)+?)\1$$"
    "foobarbar" should matchPattern { case r145("bar") => }
  }

  it should "pass test 146: ^.{2,3}?(.+)\\1$" in {
    val r146 = r"^.{2,3}?(.+)\1$$"
    "foobarbar" should matchPattern { case r146("bar") => }
  }

  it should "pass test 147: ^.{2,3}?(.+?)\\1$" in {
    val r147 = r"^.{2,3}?(.+?)\1$$"
    "foobarbar" should matchPattern { case r147("bar") => }
  }

  it should "pass test 148: ^.{3,4}((?:b|a|r)+)\\1$" in {
    val r148 = r"^.{3,4}((?:b|a|r)+)\1$$"
    "foobarbar" should matchPattern { case r148("bar") => }
  }

  it should "pass test 149: ^.{3,4}((?:b|a|r)+?)\\1$" in {
    val r149 = r"^.{3,4}((?:b|a|r)+?)\1$$"
    "foobarbar" should matchPattern { case r149("bar") => }
  }

  it should "pass test 150: ^.{3,4}(.+)\\1$" in {
    val r150 = r"^.{3,4}(.+)\1$$"
    "foobarbar" should matchPattern { case r150("bar") => }
  }

  it should "pass test 151: ^.{3,4}(.+?)\\1$" in {
    val r151 = r"^.{3,4}(.+?)\1$$"
    "foobarbar" should matchPattern { case r151("bar") => }
  }

  it should "pass test 152: ^m?(\\d)(.*)\\1$" in {
    val r152 = r"^m?(\d)(.*)\1$$"
    "5b5" should matchPattern { case r152("5", "b") => }
  }

  it should "pass test 153: ^m?(\\D)(.*)\\1$" in {
    val r153 = r"^m?(\D)(.*)\1$$"
    "aba" should matchPattern { case r153("a", "b") => }
  }

  it should "pass test 154: ^m?(\\S)(.*)\\1$" in {
    val r154 = r"^m?(\S)(.*)\1$$"
    "aba" should matchPattern { case r154("a", "b") => }
  }

  it should "pass test 155: ^m?(\\W)(.*)\\1$" in {
    val r155 = r"^m?(\W)(.*)\1$$"
    ":b:" should matchPattern { case r155(":", "b") => }
  }

  it should "pass test 156: ^m?(\\w)(.*)\\1$" in {
    val r156 = r"^m?(\w)(.*)\1$$"
    "aba" should matchPattern { case r156("a", "b") => }
  }

  it should "pass test 157: a(?:b|(c|e){1,2}?|d)+?(.)" in {
    val r157 = r"a(?:b|(c|e){1,2}?|d)+?(.)"
    "ace" should matchPattern { case r157(Some(Some("c")), "e") => }
  }

  it should "pass test 158: a(?:b|c|d)(.)" in {
    val r158 = r"a(?:b|c|d)(.)"
    "ace" should matchPattern { case r158("e") => }
  }

  it should "pass test 159: a(?:b|c|d)*(.)" in {
    val r159 = r"a(?:b|c|d)*(.)"
    "ace" should matchPattern { case r159("e") => }
  }

  it should "pass test 160: a(?:b|c|d)+(.)" in {
    val r160 = r"a(?:b|c|d)+(.)"
    "acdbcdbe" should matchPattern { case r160("e") => }
  }

  it should "pass test 161: a(?:b|c|d)+?(.)" in {
    val r161 = r"a(?:b|c|d)+?(.)"
    "acdbcdbe" should matchPattern { case r161("e") => }
  }

  it should "pass test 162: a(?:b|c|d)+?(.)" in {
    val r162 = r"a(?:b|c|d)+?(.)"
    "ace" should matchPattern { case r162("e") => }
  }

  it should "pass test 163: a(?:b|c|d){5,6}(.)" in {
    val r163 = r"a(?:b|c|d){5,6}(.)"
    "acdbcdbe" should matchPattern { case r163("e") => }
  }

  it should "pass test 164: a(?:b|c|d){5,6}?(.)" in {
    val r164 = r"a(?:b|c|d){5,6}?(.)"
    "acdbcdbe" should matchPattern { case r164("e") => }
  }

  it should "pass test 165: a(?:b|c|d){5,7}(.)" in {
    val r165 = r"a(?:b|c|d){5,7}(.)"
    "acdbcdbe" should matchPattern { case r165("e") => }
  }

  it should "pass test 166: a(?:b|c|d){5,7}?(.)" in {
    val r166 = r"a(?:b|c|d){5,7}?(.)"
    "acdbcdbe" should matchPattern { case r166("e") => }
  }

  it should "pass test 167: a(?:b|c|d){6,7}(.)" in {
    val r167 = r"a(?:b|c|d){6,7}(.)"
    "acdbcdbe" should matchPattern { case r167("e") => }
  }

  it should "pass test 168: a(?:b|c|d){6,7}?(.)" in {
    val r168 = r"a(?:b|c|d){6,7}?(.)"
    "acdbcdbe" should matchPattern { case r168("e") => }
  }

  it should "pass test 169: a([bc]*)(c*d)" in {
    val r169 = r"a([bc]*)(c*d)"
    "abcd" should matchPattern { case r169("bc", "d") => }
  }

  it should "pass test 170: a([bc]*)(c+d)" in {
    val r170 = r"a([bc]*)(c+d)"
    "abcd" should matchPattern { case r170("b", "cd") => }
  }

  it should "pass test 171: a([bc]*)c*" in {
    val r171 = r"a([bc]*)c*"
    "abc" should matchPattern { case r171("bc") => }
  }

  it should "pass test 172: a([bc]+)(c*d)" in {
    val r172 = r"a([bc]+)(c*d)"
    "abcd" should matchPattern { case r172("bc", "d") => }
  }

  it should "pass test 173: a(bc)d" in {
    val r173 = r"a(bc)d"
    "abcd" should matchPattern { case r173("bc") => }
  }

  it should "pass test 174: foo(aA)*+b" in {
    val r174 = r"foo(aA)*+b"
    "fooaAaAaAaAaAb" should matchPattern { case r174(Some("aA")) => }
  }

  it should "pass test 175: foo(aA)++b" in {
    val r175 = r"foo(aA)++b"
    "fooaAaAaAaAaAb" should matchPattern { case r175("aA") => }
  }

  it should "pass test 176: foo(aA)?+b" in {
    val r176 = r"foo(aA)?+b"
    "fooaAb" should matchPattern { case r176(Some("aA")) => }
  }

  it should "pass test 177: foo(aA){1,5}+b" in {
    val r177 = r"foo(aA){1,5}+b"
    "fooaAaAaAaAaAb" should matchPattern { case r177("aA") => }
  }

  it should "pass test 178: foo(aA|bB)*+b" in {
    val r178 = r"foo(aA|bB)*+b"
    "foobBbBaAaAaAb" should matchPattern { case r178(Some("aA")) => }
  }

  it should "pass test 179: foo(aA|bB)++b" in {
    val r179 = r"foo(aA|bB)++b"
    "foobBaAbBaAbBb" should matchPattern { case r179("bB") => }
  }

  it should "pass test 180: foo(aA|bB)?+b" in {
    val r180 = r"foo(aA|bB)?+b"
    "foobBb" should matchPattern { case r180(Some("bB")) => }
  }

  it should "pass test 181: foo(aA|bB){1,5}+b" in {
    val r181 = r"foo(aA|bB){1,5}+b"
    "foobBaAaAaAaAb" should matchPattern { case r181("aA") => }
  }

  it should "pass test 182: X(\\w+)(?=\\s)|X(\\w+)" in {
    val r182 = r"X(\w+)(?=\s)|X(\w+)"
    "Xab" should matchPattern { case r182(Right("ab")) => }
  }

  it should "pass test 183: x(~~)*(?:(?:F)?)?" in {
    val r183 = r"x(~~)*(?:(?:F)?)?"
    "x~~" should matchPattern { case r183(Some("~~")) => }
  }
}
