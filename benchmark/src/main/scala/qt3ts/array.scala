package experiments.benchmark.qt3ts

import java.util.concurrent.TimeUnit
import java.util.regex.Pattern
import org.openjdk.jmh.annotations.*

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Warmup(iterations = 8, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 8, time = 2, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ArrayQT3TSBenchmarks {

  val r1: Pattern = Pattern.compile(raw"((((((((((a))))))))))")
  val r2: Pattern = Pattern.compile(raw"((((((((((a))))))))))\10")
  val r3: Pattern = Pattern.compile(raw"(((((((((a)))))))))")
  val r4: Pattern = Pattern.compile(raw"((?:aaaa|bbbb)cccc)?")
  val r5: Pattern = Pattern.compile(raw"((?:aaaa|bbbb)cccc)?")
  val r6: Pattern = Pattern.compile(raw"((?i)a)b")
  val r7: Pattern = Pattern.compile(raw"((?i)a)b")
  val r8: Pattern = Pattern.compile(raw"((?i:a))b")
  val r9: Pattern = Pattern.compile(raw"((?i:a))b")
  val r10: Pattern = Pattern.compile(raw"(([a-c])b*?\2)*")
  val r11: Pattern = Pattern.compile(raw"(([a-c])b*?\2){3}")
  val r12: Pattern = Pattern.compile(raw"((a)(b)c)(d)")
  val r13: Pattern = Pattern.compile(raw"((foo)|(bar))*")
  val r14: Pattern = Pattern.compile(raw"(.*)c(.*)")
  val r15: Pattern = Pattern.compile(raw"(?:(f)(o)(o)|(b)(a)(r))*")
  val r22: Pattern = Pattern.compile(raw"([a-c]*)\1")
  val r23: Pattern = Pattern.compile(raw"([abc])*bcd")
  val r24: Pattern = Pattern.compile(raw"([abc])*d")
  val r25: Pattern = Pattern.compile(raw"([yX].|WORDS|[yX].|WORD)+S")
  val r26: Pattern = Pattern.compile(raw"([yX].|WORDS|[yX].|WORD)S")
  val r27: Pattern = Pattern.compile(raw"([yX].|WORDS|WORD|[xY].)+S")
  val r28: Pattern = Pattern.compile(raw"([yX].|WORDS|WORD|[xY].)S")
  val r29: Pattern = Pattern.compile(raw"([zx].|foo|fool|[zq].|money|parted|[yx].)$$")
  val r30: Pattern = Pattern.compile(raw"([zx].|foo|fool|[zq].|money|parted|[yx].)+$$")
  val r31: Pattern = Pattern.compile(raw"(\d+\.\d+)")
  val r32: Pattern = Pattern.compile(raw"(\w+:)+")
  val r33: Pattern = Pattern.compile(raw"(^|a)b")
  val r34: Pattern = Pattern.compile(raw"(a)?(a)+")
  val r35: Pattern = Pattern.compile(raw"(a)b(c)")
  val r36: Pattern = Pattern.compile(raw"(a)|(b)")
  val r37: Pattern = Pattern.compile(raw"(a)|\1")
  val r38: Pattern = Pattern.compile(raw"(a+|b)*")
  val r39: Pattern = Pattern.compile(raw"(a+|b)+")
  val r40: Pattern = Pattern.compile(raw"(a+|b){0,}")
  val r41: Pattern = Pattern.compile(raw"(a+|b){1,}")
  val r42: Pattern = Pattern.compile(raw"(aA)*+b")
  val r43: Pattern = Pattern.compile(raw"(aA)++b")
  val r44: Pattern = Pattern.compile(raw"(aA)?+b")
  val r45: Pattern = Pattern.compile(raw"(aA){1,5}+b")
  val r46: Pattern = Pattern.compile(raw"(aA|bB)*+b")
  val r47: Pattern = Pattern.compile(raw"(aA|bB)++b")
  val r48: Pattern = Pattern.compile(raw"(aA|bB)?+b")
  val r49: Pattern = Pattern.compile(raw"(aA|bB){1,5}+b")
  val r50: Pattern = Pattern.compile(raw"(ab)?(ab)+")
  val r51: Pattern = Pattern.compile(raw"(abc)?(abc)+")
  val r52: Pattern = Pattern.compile(raw"(abc)\1")
  val r53: Pattern = Pattern.compile(raw"(ab|a)b*c")
  val r54: Pattern = Pattern.compile(raw"(ab|ab*)bc")
  val r55: Pattern = Pattern.compile(raw"(a|(bc)){0,0}+xyz")
  val r56: Pattern = Pattern.compile(raw"(a|(bc)){0,0}?xyz")
  val r57: Pattern = Pattern.compile(raw"(a|b|c|d|e)f")
  val r58: Pattern = Pattern.compile(raw"(bc+d$$|ef*g.|h?i(j|k))")
  val r59: Pattern = Pattern.compile(raw"(bc+d$$|ef*g.|h?i(j|k))")
  val r60: Pattern = Pattern.compile(raw"(foo[1x]|bar[2x]|baz[3x])*y")
  val r61: Pattern = Pattern.compile(raw"(foo[1x]|bar[2x]|baz[3x])+y")
  val r62: Pattern = Pattern.compile(raw"(foo|fool|[zx].|money|parted)$$")
  val r63: Pattern = Pattern.compile(raw"(foo|fool|[zx].|money|parted)+$$")
  val r64: Pattern = Pattern.compile(raw"(foo|fool|money|parted)$$")
  val r65: Pattern = Pattern.compile(raw"(foo|fool|x.|money|parted)$$")
  val r66: Pattern = Pattern.compile(raw"(q1|.)*(q2|.)*(x(a|bc)*y){2,3}")
  val r67: Pattern = Pattern.compile(raw"(q1|.)*(q2|.)*(x(a|bc)*y){2,}")
  val r68: Pattern = Pattern.compile(raw"(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z")
  val r69: Pattern = Pattern.compile(raw"(WORDS|WORD)S")
  val r70: Pattern = Pattern.compile(raw"(WORDS|WORLD|WORD)+S")
  val r71: Pattern = Pattern.compile(raw"(WORDS|WORLD|WORD)S")
  val r72: Pattern = Pattern.compile(raw"(x.|foo|fool|x.|money|parted|y.)$$")
  val r73: Pattern = Pattern.compile(raw"(X.|WORDS|WORD|Y.)S")
  val r74: Pattern = Pattern.compile(raw"(X.|WORDS|X.|WORD)S")
  val r75: Pattern = Pattern.compile(raw"(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*")
  val r76: Pattern = Pattern.compile(raw"(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+")
  val r77: Pattern = Pattern.compile(raw"(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+")
  val r78: Pattern = Pattern.compile(raw"(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++")
  val r79: Pattern = Pattern.compile(raw"(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}")
  val r80: Pattern = Pattern.compile(raw"(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+")
  val r81: Pattern = Pattern.compile(raw".*?(?:(\w)|(\w))x")
  val r82: Pattern = Pattern.compile(raw"2(]*)?$$\1")
  val r83: Pattern = Pattern.compile(raw"\((.*), (.*)\)")
  val r84: Pattern = Pattern.compile(raw"^((?:aa)*)(?:X+((?:\d+|-)(?:X+(.+))?))?$$")
  val r85: Pattern = Pattern.compile(raw"^((a|b)+)*ax")
  val r86: Pattern = Pattern.compile(raw"^((a|bc)+)*ax")
  val r88: Pattern = Pattern.compile(raw"^(.+)?B")
  val r89: Pattern = Pattern.compile(raw"^(.,){2}c")
  val r90: Pattern = Pattern.compile(raw"^(0+)?(?:x(1))?")
  val r91: Pattern = Pattern.compile(raw"^(?:(\d)x)?\d$$")
  val r92: Pattern = Pattern.compile(raw"^(?:(X)?(\d)|(X)?(\d\d))$$")
  val r93: Pattern = Pattern.compile(raw"^(?:(XX)?(\d)|(XX)?(\d\d))$$")
  val r94: Pattern = Pattern.compile(raw"^(?:f|o|b){2,3}?((?:b|a|r)+)\1$$")
  val r95: Pattern = Pattern.compile(raw"^(?:f|o|b){2,3}?((?:b|a|r)+?)\1$$")
  val r96: Pattern = Pattern.compile(raw"^(?:f|o|b){2,3}?(.+)\1$$")
  val r97: Pattern = Pattern.compile(raw"^(?:f|o|b){2,3}?(.+?)\1$$")
  val r98: Pattern = Pattern.compile(raw"^(?:f|o|b){3,4}((?:b|a|r)+)\1$$")
  val r99: Pattern = Pattern.compile(raw"^(?:f|o|b){3,4}((?:b|a|r)+?)\1$$")
  val r100: Pattern = Pattern.compile(raw"^(?:f|o|b){3,4}(.+)\1$$")
  val r101: Pattern = Pattern.compile(raw"^(?:f|o|b){3,4}(.+?)\1$$")
  val r102: Pattern = Pattern.compile(raw"^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?")
  val r103: Pattern = Pattern.compile(raw"^([^,]*,){0,3}d")
  val r104: Pattern = Pattern.compile(raw"^([^,]*,){2}c")
  val r105: Pattern = Pattern.compile(raw"^([^,]*,){3,}d")
  val r106: Pattern = Pattern.compile(raw"^([^,]*,){3}d")
  val r107: Pattern = Pattern.compile(raw"^([^,]{0,3},){0,3}d")
  val r108: Pattern = Pattern.compile(raw"^([^,]{0,3},){3,}d")
  val r109: Pattern = Pattern.compile(raw"^([^,]{0,3},){3}d")
  val r110: Pattern = Pattern.compile(raw"^([^,]{1,3},){0,3}d")
  val r111: Pattern = Pattern.compile(raw"^([^,]{1,3},){3,}d")
  val r112: Pattern = Pattern.compile(raw"^([^,]{1,3},){3}d")
  val r113: Pattern = Pattern.compile(raw"^([^,]{1,},){0,3}d")
  val r114: Pattern = Pattern.compile(raw"^([^,]{1,},){3,}d")
  val r115: Pattern = Pattern.compile(raw"^([^,]{1,},){3}d")
  val r116: Pattern = Pattern.compile(raw"^([^a-z])|(\^)$$")
  val r117: Pattern = Pattern.compile(raw"^([a]{1})*$$")
  val r118: Pattern = Pattern.compile(raw"^([ab]*?)(b)?(c)$$")
  val r119: Pattern = Pattern.compile(raw"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):")
  val r120: Pattern = Pattern.compile(raw"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):")
  val r121: Pattern = Pattern.compile(raw"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):")
  val r122: Pattern = Pattern.compile(raw"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):")
  val r123: Pattern = Pattern.compile(raw"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):")
  val r124: Pattern = Pattern.compile(raw"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):")
  val r125: Pattern = Pattern.compile(raw"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):")
  val r126: Pattern = Pattern.compile(raw"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):")
  val r127: Pattern = Pattern.compile(raw"^(a(b)?)+$$")
  val r128: Pattern = Pattern.compile(raw"^(a)?a$$")
  val r129: Pattern = Pattern.compile(raw"^(a+)*ax")
  val r130: Pattern = Pattern.compile(raw"^(a\1?)(a\1?)(a\2?)(a\3?)$$")
  val r131: Pattern = Pattern.compile(raw"^(a\1?){4}$$")
  val r132: Pattern = Pattern.compile(raw"^(a\1?){4}$$")
  val r133: Pattern = Pattern.compile(raw"^(aa(bb)?)+$$")
  val r134: Pattern = Pattern.compile(raw"^(b+?|a){1,2}c")
  val r135: Pattern = Pattern.compile(raw"^(b+?|a){1,2}c")
  val r140: Pattern = Pattern.compile(raw"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):")
  val r141: Pattern = Pattern.compile(raw"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):")
  val r142: Pattern = Pattern.compile(raw"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):")
  val r143: Pattern = Pattern.compile(raw"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):")
  val r144: Pattern = Pattern.compile(raw"^.{2,3}?((?:b|a|r)+)\1$$")
  val r145: Pattern = Pattern.compile(raw"^.{2,3}?((?:b|a|r)+?)\1$$")
  val r146: Pattern = Pattern.compile(raw"^.{2,3}?(.+)\1$$")
  val r147: Pattern = Pattern.compile(raw"^.{2,3}?(.+?)\1$$")
  val r148: Pattern = Pattern.compile(raw"^.{3,4}((?:b|a|r)+)\1$$")
  val r149: Pattern = Pattern.compile(raw"^.{3,4}((?:b|a|r)+?)\1$$")
  val r150: Pattern = Pattern.compile(raw"^.{3,4}(.+)\1$$")
  val r151: Pattern = Pattern.compile(raw"^.{3,4}(.+?)\1$$")
  val r152: Pattern = Pattern.compile(raw"^m?(\d)(.*)\1$$")
  val r153: Pattern = Pattern.compile(raw"^m?(\D)(.*)\1$$")
  val r154: Pattern = Pattern.compile(raw"^m?(\S)(.*)\1$$")
  val r155: Pattern = Pattern.compile(raw"^m?(\W)(.*)\1$$")
  val r156: Pattern = Pattern.compile(raw"^m?(\w)(.*)\1$$")
  val r157: Pattern = Pattern.compile(raw"a(?:b|(c|e){1,2}?|d)+?(.)")
  val r158: Pattern = Pattern.compile(raw"a(?:b|c|d)(.)")
  val r159: Pattern = Pattern.compile(raw"a(?:b|c|d)*(.)")
  val r160: Pattern = Pattern.compile(raw"a(?:b|c|d)+(.)")
  val r161: Pattern = Pattern.compile(raw"a(?:b|c|d)+?(.)")
  val r162: Pattern = Pattern.compile(raw"a(?:b|c|d)+?(.)")
  val r163: Pattern = Pattern.compile(raw"a(?:b|c|d){5,6}(.)")
  val r164: Pattern = Pattern.compile(raw"a(?:b|c|d){5,6}?(.)")
  val r165: Pattern = Pattern.compile(raw"a(?:b|c|d){5,7}(.)")
  val r166: Pattern = Pattern.compile(raw"a(?:b|c|d){5,7}?(.)")
  val r167: Pattern = Pattern.compile(raw"a(?:b|c|d){6,7}(.)")
  val r168: Pattern = Pattern.compile(raw"a(?:b|c|d){6,7}?(.)")
  val r169: Pattern = Pattern.compile(raw"a([bc]*)(c*d)")
  val r170: Pattern = Pattern.compile(raw"a([bc]*)(c+d)")
  val r171: Pattern = Pattern.compile(raw"a([bc]*)c*")
  val r172: Pattern = Pattern.compile(raw"a([bc]+)(c*d)")
  val r173: Pattern = Pattern.compile(raw"a(bc)d")
  val r174: Pattern = Pattern.compile(raw"foo(aA)*+b")
  val r175: Pattern = Pattern.compile(raw"foo(aA)++b")
  val r176: Pattern = Pattern.compile(raw"foo(aA)?+b")
  val r177: Pattern = Pattern.compile(raw"foo(aA){1,5}+b")
  val r178: Pattern = Pattern.compile(raw"foo(aA|bB)*+b")
  val r179: Pattern = Pattern.compile(raw"foo(aA|bB)++b")
  val r180: Pattern = Pattern.compile(raw"foo(aA|bB)?+b")
  val r181: Pattern = Pattern.compile(raw"foo(aA|bB){1,5}+b")
  val r182: Pattern = Pattern.compile(raw"X(\w+)(?=\s)|X(\w+)")
  val r183: Pattern = Pattern.compile(raw"x(~~)*(?:(?:F)?)?")

  def arrayOfOptions(pattern: Pattern, s: String): Option[Array[Option[String]]] = {
    val matcher = pattern.matcher(s)
    if (matcher.matches) {
      Some(Array.tabulate(matcher.groupCount) { i => Option(matcher.group(i + 1)) })
    } else None
  }

  @Benchmark
  def benchmark1 = arrayOfOptions(r1, "a")

  @Benchmark
  def benchmark2 = arrayOfOptions(r2, "aa")

  @Benchmark
  def benchmark3 = arrayOfOptions(r3, "a")

  @Benchmark
  def benchmark4 = arrayOfOptions(r4, "aaaacccc")

  @Benchmark
  def benchmark5 = arrayOfOptions(r5, "bbbbcccc")

  @Benchmark
  def benchmark6 = arrayOfOptions(r6, "ab")

  @Benchmark
  def benchmark7 = arrayOfOptions(r7, "Ab")

  @Benchmark
  def benchmark8 = arrayOfOptions(r8, "ab")

  @Benchmark
  def benchmark9 = arrayOfOptions(r9, "Ab")

  @Benchmark
  def benchmark10 = arrayOfOptions(r10, "ababbbcbc")

  @Benchmark
  def benchmark11 = arrayOfOptions(r11, "ababbbcbc")

  @Benchmark
  def benchmark12 = arrayOfOptions(r12, "abcd")

  @Benchmark
  def benchmark13 = arrayOfOptions(r13, "foobar")

  @Benchmark
  def benchmark14 = arrayOfOptions(r14, "abcde")

  @Benchmark
  def benchmark15 = arrayOfOptions(r15, "foobar")

  // it should "pass test 16: ([[:digit:]-[:alpha:]]+)" in pending // {
  //   val r16 = raw"([[:digit:]-[:alpha:]]+)".r
  //   "-" should matchPattern { case r16("-") => }
  // }

  // it should "pass test 17: ([[:digit:]-z]+)" in pending // {
  //   val r17 = raw"([[:digit:]-z]+)".r
  //   "-" should matchPattern { case r17("-") => }
  // }

  // TODO: Parser bug
  // it should "pass test 18: ([\\d-\\s]+)" in pending // {
  //   val r18 = raw"([\\d-\\s]+)".r
  //   "-" should matchPattern { case r18("-") => }
  // }

  // TODO: Parser bug
  // it should "pass test 19: ([\\d-z]+)" in pending // {
  //   val r19 = raw"([\\d-z]+)".r
  //   "-" should matchPattern { case r19("-") => }
  // }

  // TODO: Parser bug
  // it should "pass test 20: ([\\w:]+::)?(\\w+)$" in pending // {
  //   val r20 = raw"([\w:]+::)?(\w+)$$".r
  //   "abcd" should matchPattern { case r20(None, "abcd") => }
  // }

  // TODO: Parser bug
  // it should "pass test 21: ([\\w:]+::)?(\\w+)$" in pending // {
  //   val r21 = raw"([\w:]+::)?(\w+)$$".r
  //   "xy:z:::abcd" should matchPattern { case r21(Some("xy:z:::"), "abcd") => }
  // }

  @Benchmark
  def benchmark22 = arrayOfOptions(r22, "abcabc")

  @Benchmark
  def benchmark23 = arrayOfOptions(r23, "abcd")

  @Benchmark
  def benchmark24 = arrayOfOptions(r24, "abbbcd")

  @Benchmark
  def benchmark25 = arrayOfOptions(r25, "WORDS")

  @Benchmark
  def benchmark26 = arrayOfOptions(r26, "WORDS")

  @Benchmark
  def benchmark27 = arrayOfOptions(r27, "WORDS")

  @Benchmark
  def benchmark28 = arrayOfOptions(r28, "WORDS")

  @Benchmark
  def benchmark29 = arrayOfOptions(r29, "fool")

  @Benchmark
  def benchmark30 = arrayOfOptions(r30, "fool")

  @Benchmark
  def benchmark31 = arrayOfOptions(r31, "3.1415926")

  @Benchmark
  def benchmark32 = arrayOfOptions(r32, "one:")

  @Benchmark
  def benchmark33 = arrayOfOptions(r33, "ab")

  @Benchmark
  def benchmark34 = arrayOfOptions(r34, "a")

  @Benchmark
  def benchmark35 = arrayOfOptions(r35, "abc")

  @Benchmark
  def benchmark36 = arrayOfOptions(r36, "b")

  @Benchmark
  def benchmark37 = arrayOfOptions(r37, "a")

  @Benchmark
  def benchmark38 = arrayOfOptions(r38, "ab")

  @Benchmark
  def benchmark39 = arrayOfOptions(r39, "ab")

  @Benchmark
  def benchmark40 = arrayOfOptions(r40, "ab")

  @Benchmark
  def benchmark41 = arrayOfOptions(r41, "ab")

  @Benchmark
  def benchmark42 = arrayOfOptions(r42, "aAaAaAaAaAb")

  @Benchmark
  def benchmark43 = arrayOfOptions(r43, "aAaAaAaAaAb")

  @Benchmark
  def benchmark44 = arrayOfOptions(r44, "aAb")

  @Benchmark
  def benchmark45 = arrayOfOptions(r45, "aAaAaAaAaAb")

  @Benchmark
  def benchmark46 = arrayOfOptions(r46, "bBbBbBbBbBb")

  @Benchmark
  def benchmark47 = arrayOfOptions(r47, "aAbBaAaAbBb")

  @Benchmark
  def benchmark48 = arrayOfOptions(r48, "bBb")

  @Benchmark
  def benchmark49 = arrayOfOptions(r49, "bBaAbBaAbBb")

  @Benchmark
  def benchmark50 = arrayOfOptions(r50, "ab")

  @Benchmark
  def benchmark51 = arrayOfOptions(r51, "abc")

  @Benchmark
  def benchmark52 = arrayOfOptions(r52, "abcabc")

  @Benchmark
  def benchmark53 = arrayOfOptions(r53, "abc")

  @Benchmark
  def benchmark54 = arrayOfOptions(r54, "abc")

  @Benchmark
  def benchmark55 = arrayOfOptions(r55, "xyz")

  @Benchmark
  def benchmark56 = arrayOfOptions(r56, "xyz")

  @Benchmark
  def benchmark57 = arrayOfOptions(r57, "ef")

  @Benchmark
  def benchmark58 = arrayOfOptions(r58, "effgz")

  @Benchmark
  def benchmark59 = arrayOfOptions(r59, "ij")

  @Benchmark
  def benchmark60 = arrayOfOptions(r60, "foo1bar2baz3y")

  @Benchmark
  def benchmark61 = arrayOfOptions(r61, "foo1bar2baz3y")

  @Benchmark
  def benchmark62 = arrayOfOptions(r62, "fool")

  @Benchmark
  def benchmark63 = arrayOfOptions(r63, "fool")

  @Benchmark
  def benchmark64 = arrayOfOptions(r64, "fool")

  @Benchmark
  def benchmark65 = arrayOfOptions(r65, "fool")

  @Benchmark
  def benchmark66 = arrayOfOptions(r66, "xayxay")

  @Benchmark
  def benchmark67 = arrayOfOptions(r67, "xayxay")

  @Benchmark
  def benchmark68 = arrayOfOptions(r68, "zzzzzzzzzzzzzzzz-xayxayxayxayZ")

  @Benchmark
  def benchmark69 = arrayOfOptions(r69, "WORDS")

  @Benchmark
  def benchmark70 = arrayOfOptions(r70, "WORDS")

  @Benchmark
  def benchmark71 = arrayOfOptions(r71, "WORDS")

  @Benchmark
  def benchmark72 = arrayOfOptions(r72, "fool")

  @Benchmark
  def benchmark73 = arrayOfOptions(r73, "WORDS")

  @Benchmark
  def benchmark74 = arrayOfOptions(r74, "WORDS")

  @Benchmark
  def benchmark75 = arrayOfOptions(r75, "xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark76 = arrayOfOptions(r76, "xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark77 = arrayOfOptions(r77, "xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark78 = arrayOfOptions(r78, "xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark79 = arrayOfOptions(r79, "xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark80 = arrayOfOptions(r80, "xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark81 = arrayOfOptions(r81, "abx")

  @Benchmark
  def benchmark82 = arrayOfOptions(r82, "2")

  @Benchmark
  def benchmark83 = arrayOfOptions(r83, "(a, b)")

  @Benchmark
  def benchmark84 = arrayOfOptions(r84, "aaaaX5")

  @Benchmark
  def benchmark85 = arrayOfOptions(r85, "aax")

  @Benchmark
  def benchmark86 = arrayOfOptions(r86, "aax")

  // it should "pass test 87: ^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$" in pending // {
  //   val r87 = raw"^(.*?)\s*\|\s*(?:\/\s*|)'(.+)'$$".r
  //   "text|\'sec\'" should matchPattern { case r87("text", "sec") => }
  // }

  @Benchmark
  def benchmark88 = arrayOfOptions(r88, "AB")

  @Benchmark
  def benchmark89 = arrayOfOptions(r89, "a,b,c")

  @Benchmark
  def benchmark90 = arrayOfOptions(r90, "x1")

  @Benchmark
  def benchmark91 = arrayOfOptions(r91, "1")

  @Benchmark
  def benchmark92 = arrayOfOptions(r92, "X12")

  @Benchmark
  def benchmark93 = arrayOfOptions(r93, "XX12")

  @Benchmark
  def benchmark94 = arrayOfOptions(r94, "foobarbar")

  @Benchmark
  def benchmark95 = arrayOfOptions(r95, "foobarbar")

  @Benchmark
  def benchmark96 = arrayOfOptions(r96, "foobarbar")

  @Benchmark
  def benchmark97 = arrayOfOptions(r97, "foobarbar")

  @Benchmark
  def benchmark98 = arrayOfOptions(r98, "foobarbar")

  @Benchmark
  def benchmark99 = arrayOfOptions(r99, "foobarbar")

  @Benchmark
  def benchmark100 = arrayOfOptions(r100, "foobarbar")

  @Benchmark
  def benchmark101 = arrayOfOptions(r101, "foobarbar")

  @Benchmark
  def benchmark102 = arrayOfOptions(r102, "012cxx0190")

  @Benchmark
  def benchmark103 = arrayOfOptions(r103, "aaa,b,c,d")

  @Benchmark
  def benchmark104 = arrayOfOptions(r104, "a,b,c")

  @Benchmark
  def benchmark105 = arrayOfOptions(r105, "aaa,b,c,d")

  @Benchmark
  def benchmark106 = arrayOfOptions(r106, "aaa,b,c,d")

  @Benchmark
  def benchmark107 = arrayOfOptions(r107, "aaa,b,c,d")

  @Benchmark
  def benchmark108 = arrayOfOptions(r108, "aaa,b,c,d")

  @Benchmark
  def benchmark109 = arrayOfOptions(r109, "aaa,b,c,d")

  @Benchmark
  def benchmark110 = arrayOfOptions(r110, "aaa,b,c,d")

  @Benchmark
  def benchmark111 = arrayOfOptions(r111, "aaa,b,c,d")

  @Benchmark
  def benchmark112 = arrayOfOptions(r112, "aaa,b,c,d")

  @Benchmark
  def benchmark113 = arrayOfOptions(r113, "aaa,b,c,d")

  @Benchmark
  def benchmark114 = arrayOfOptions(r114, "aaa,b,c,d")

  @Benchmark
  def benchmark115 = arrayOfOptions(r115, "aaa,b,c,d")

  @Benchmark
  def benchmark116 = arrayOfOptions(r116, ".")

  @Benchmark
  def benchmark117 = arrayOfOptions(r117, "aa")

  @Benchmark
  def benchmark118 = arrayOfOptions(r118, "abac")

  @Benchmark
  def benchmark119 = arrayOfOptions(r119, "ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark120 = arrayOfOptions(r120, "ZEQQQX:")

  @Benchmark
  def benchmark121 = arrayOfOptions(r121, "ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark122 = arrayOfOptions(r122, "ZEQQQX:")

  @Benchmark
  def benchmark123 = arrayOfOptions(r123, "ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark124 = arrayOfOptions(r124, "ZEQQQX:")

  @Benchmark
  def benchmark125 = arrayOfOptions(r125, "ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark126 = arrayOfOptions(r126, "ZEQQQX:")

  @Benchmark
  def benchmark127 = arrayOfOptions(r127, "aba")

  @Benchmark
  def benchmark128 = arrayOfOptions(r128, "a")

  @Benchmark
  def benchmark129 = arrayOfOptions(r129, "aax")

  @Benchmark
  def benchmark130 = arrayOfOptions(r130, "aaaaaa")

  @Benchmark
  def benchmark131 = arrayOfOptions(r131, "aaaaaa")

  @Benchmark
  def benchmark132 = arrayOfOptions(r132, "aaaaaaaaaa")

  @Benchmark
  def benchmark133 = arrayOfOptions(r133, "aabbaa")

  @Benchmark
  def benchmark134 = arrayOfOptions(r134, "bbbac")

  @Benchmark
  def benchmark135 = arrayOfOptions(r135, "bbbbac")

  // it should "pass test 136: ^(foo|)bar$" in pending // {
  //   val r136 = raw"^(foo|)bar$".r
  //   "baraw" should matchPattern { case r136("".r) => }
  // }

  // it should "pass test 137: ^(foo||baz)bar$" in pending // {
  //   val r137 = raw"^(foo||baz)bar$".r
  //   "baraw" should matchPattern { case r137("".r) => }
  // }

  // it should "pass test 138: ^(foo||baz)bar$" in pending // {
  //   val r138 = raw"^(foo||baz)bar$".r
  //   "bazbaraw" should matchPattern { case r138("baz".r) => }
  // }

  // it should "pass test 139: ^(foo||baz)bar$" in pending // {
  //   val r139 = raw"^(foo||baz)bar$".r
  //   "foobaraw" should matchPattern { case r139("foo".r) => }
  // }

  @Benchmark
  def benchmark140 = arrayOfOptions(r140, "ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark141 = arrayOfOptions(r141, "ZEQQQX:")

  @Benchmark
  def benchmark142 = arrayOfOptions(r142, "ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark143 = arrayOfOptions(r143, "ZEQQQX:")

  @Benchmark
  def benchmark144 = arrayOfOptions(r144, "foobarbar")

  @Benchmark
  def benchmark145 = arrayOfOptions(r145, "foobarbar")

  @Benchmark
  def benchmark146 = arrayOfOptions(r146, "foobarbar")

  @Benchmark
  def benchmark147 = arrayOfOptions(r147, "foobarbar")

  @Benchmark
  def benchmark148 = arrayOfOptions(r148, "foobarbar")

  @Benchmark
  def benchmark149 = arrayOfOptions(r149, "foobarbar")

  @Benchmark
  def benchmark150 = arrayOfOptions(r150, "foobarbar")

  @Benchmark
  def benchmark151 = arrayOfOptions(r151, "foobarbar")

  @Benchmark
  def benchmark152 = arrayOfOptions(r152, "5b5")

  @Benchmark
  def benchmark153 = arrayOfOptions(r153, "aba")

  @Benchmark
  def benchmark154 = arrayOfOptions(r154, "aba")

  @Benchmark
  def benchmark155 = arrayOfOptions(r155, ":b:")

  @Benchmark
  def benchmark156 = arrayOfOptions(r156, "aba")

  @Benchmark
  def benchmark157 = arrayOfOptions(r157, "ace")

  @Benchmark
  def benchmark158 = arrayOfOptions(r158, "ace")

  @Benchmark
  def benchmark159 = arrayOfOptions(r159, "ace")

  @Benchmark
  def benchmark160 = arrayOfOptions(r160, "acdbcdbe")

  @Benchmark
  def benchmark161 = arrayOfOptions(r161, "acdbcdbe")

  @Benchmark
  def benchmark162 = arrayOfOptions(r162, "ace")

  @Benchmark
  def benchmark163 = arrayOfOptions(r163, "acdbcdbe")

  @Benchmark
  def benchmark164 = arrayOfOptions(r164, "acdbcdbe")

  @Benchmark
  def benchmark165 = arrayOfOptions(r165, "acdbcdbe")

  @Benchmark
  def benchmark166 = arrayOfOptions(r166, "acdbcdbe")

  @Benchmark
  def benchmark167 = arrayOfOptions(r167, "acdbcdbe")

  @Benchmark
  def benchmark168 = arrayOfOptions(r168, "acdbcdbe")

  @Benchmark
  def benchmark169 = arrayOfOptions(r169, "abcd")

  @Benchmark
  def benchmark170 = arrayOfOptions(r170, "abcd")

  @Benchmark
  def benchmark171 = arrayOfOptions(r171, "abc")

  @Benchmark
  def benchmark172 = arrayOfOptions(r172, "abcd")

  @Benchmark
  def benchmark173 = arrayOfOptions(r173, "abcd")

  @Benchmark
  def benchmark174 = arrayOfOptions(r174, "fooaAaAaAaAaAb")

  @Benchmark
  def benchmark175 = arrayOfOptions(r175, "fooaAaAaAaAaAb")

  @Benchmark
  def benchmark176 = arrayOfOptions(r176, "fooaAb")

  @Benchmark
  def benchmark177 = arrayOfOptions(r177, "fooaAaAaAaAaAb")

  @Benchmark
  def benchmark178 = arrayOfOptions(r178, "foobBbBaAaAaAb")

  @Benchmark
  def benchmark179 = arrayOfOptions(r179, "foobBaAbBaAbBb")

  @Benchmark
  def benchmark180 = arrayOfOptions(r180, "foobBb")

  @Benchmark
  def benchmark181 = arrayOfOptions(r181, "foobBaAaAaAaAb")

  @Benchmark
  def benchmark182 = arrayOfOptions(r182, "Xab")

  @Benchmark
  def benchmark183 = arrayOfOptions(r183, "x~~")
}
