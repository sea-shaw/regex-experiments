package experiments.benchmark.qt3ts

import experiments.macros.oregano.r
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Warmup(iterations = 8, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 8, time = 2, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class OreganoQT3TSBenchmarks {

  val r1 = r"((((((((((a))))))))))"
  val r2 = r"((((((((((a))))))))))\10"
  val r3 = r"(((((((((a)))))))))"
  val r4 = r"((?:aaaa|bbbb)cccc)?"
  val r5 = r"((?:aaaa|bbbb)cccc)?"
  val r6 = r"((?i)a)b"
  val r7 = r"((?i)a)b"
  val r8 = r"((?i:a))b"
  val r9 = r"((?i:a))b"
  val r10 = r"(([a-c])b*?\2)*"
  val r11 = r"(([a-c])b*?\2){3}"
  val r12 = r"((a)(b)c)(d)"
  val r13 = r"((foo)|(bar))*"
  val r14 = r"(.*)c(.*)"
  val r15 = r"(?:(f)(o)(o)|(b)(a)(r))*"
  val r22 = r"([a-c]*)\1"
  val r23 = r"([abc])*bcd"
  val r24 = r"([abc])*d"
  val r25 = r"([yX].|WORDS|[yX].|WORD)+S"
  val r26 = r"([yX].|WORDS|[yX].|WORD)S"
  val r27 = r"([yX].|WORDS|WORD|[xY].)+S"
  val r28 = r"([yX].|WORDS|WORD|[xY].)S"
  val r29 = r"([zx].|foo|fool|[zq].|money|parted|[yx].)$$"
  val r30 = r"([zx].|foo|fool|[zq].|money|parted|[yx].)+$$"
  val r31 = r"(\d+\.\d+)"
  val r32 = r"(\w+:)+"
  val r33 = r"(^|a)b"
  val r34 = r"(a)?(a)+"
  val r35 = r"(a)b(c)"
  val r36 = r"(a)|(b)"
  val r37 = r"(a)|\1"
  val r38 = r"(a+|b)*"
  val r39 = r"(a+|b)+"
  val r40 = r"(a+|b){0,}"
  val r41 = r"(a+|b){1,}"
  val r42 = r"(aA)*+b"
  val r43 = r"(aA)++b"
  val r44 = r"(aA)?+b"
  val r45 = r"(aA){1,5}+b"
  val r46 = r"(aA|bB)*+b"
  val r47 = r"(aA|bB)++b"
  val r48 = r"(aA|bB)?+b"
  val r49 = r"(aA|bB){1,5}+b"
  val r50 = r"(ab)?(ab)+"
  val r51 = r"(abc)?(abc)+"
  val r52 = r"(abc)\1"
  val r53 = r"(ab|a)b*c"
  val r54 = r"(ab|ab*)bc"
  val r55 = r"(a|(bc)){0,0}+xyz"
  val r56 = r"(a|(bc)){0,0}?xyz"
  val r57 = r"(a|b|c|d|e)f"
  val r58 = r"(bc+d$$|ef*g.|h?i(j|k))"
  val r59 = r"(bc+d$$|ef*g.|h?i(j|k))"
  val r60 = r"(foo[1x]|bar[2x]|baz[3x])*y"
  val r61 = r"(foo[1x]|bar[2x]|baz[3x])+y"
  val r62 = r"(foo|fool|[zx].|money|parted)$$"
  val r63 = r"(foo|fool|[zx].|money|parted)+$$"
  val r64 = r"(foo|fool|money|parted)$$"
  val r65 = r"(foo|fool|x.|money|parted)$$"
  val r66 = r"(q1|.)*(q2|.)*(x(a|bc)*y){2,3}"
  val r67 = r"(q1|.)*(q2|.)*(x(a|bc)*y){2,}"
  val r68 = r"(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z"
  val r69 = r"(WORDS|WORD)S"
  val r70 = r"(WORDS|WORLD|WORD)+S"
  val r71 = r"(WORDS|WORLD|WORD)S"
  val r72 = r"(x.|foo|fool|x.|money|parted|y.)$$"
  val r73 = r"(X.|WORDS|WORD|Y.)S"
  val r74 = r"(X.|WORDS|X.|WORD)S"
  val r75 = r"(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*"
  val r76 = r"(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+"
  val r77 = r"(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+"
  val r78 = r"(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++"
  val r79 = r"(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}"
  val r80 = r"(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+"
  val r81 = r".*?(?:(\w)|(\w))x"
  val r82 = r"2(]*)?$$\1"
  val r83 = r"\((.*), (.*)\)"
  val r84 = r"^((?:aa)*)(?:X+((?:\d+|-)(?:X+(.+))?))?$$"
  val r85 = r"^((a|b)+)*ax"
  val r86 = r"^((a|bc)+)*ax"
  val r88 = r"^(.+)?B"
  val r89 = r"^(.,){2}c"
  val r90 = r"^(0+)?(?:x(1))?"
  val r91 = r"^(?:(\d)x)?\d$$"
  val r92 = r"^(?:(X)?(\d)|(X)?(\d\d))$$"
  val r93 = r"^(?:(XX)?(\d)|(XX)?(\d\d))$$"
  val r94 = r"^(?:f|o|b){2,3}?((?:b|a|r)+)\1$$"
  val r95 = r"^(?:f|o|b){2,3}?((?:b|a|r)+?)\1$$"
  val r96 = r"^(?:f|o|b){2,3}?(.+)\1$$"
  val r97 = r"^(?:f|o|b){2,3}?(.+?)\1$$"
  val r98 = r"^(?:f|o|b){3,4}((?:b|a|r)+)\1$$"
  val r99 = r"^(?:f|o|b){3,4}((?:b|a|r)+?)\1$$"
  val r100 = r"^(?:f|o|b){3,4}(.+)\1$$"
  val r101 = r"^(?:f|o|b){3,4}(.+?)\1$$"
  val r102 = r"^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?"
  val r103 = r"^([^,]*,){0,3}d"
  val r104 = r"^([^,]*,){2}c"
  val r105 = r"^([^,]*,){3,}d"
  val r106 = r"^([^,]*,){3}d"
  val r107 = r"^([^,]{0,3},){0,3}d"
  val r108 = r"^([^,]{0,3},){3,}d"
  val r109 = r"^([^,]{0,3},){3}d"
  val r110 = r"^([^,]{1,3},){0,3}d"
  val r111 = r"^([^,]{1,3},){3,}d"
  val r112 = r"^([^,]{1,3},){3}d"
  val r113 = r"^([^,]{1,},){0,3}d"
  val r114 = r"^([^,]{1,},){3,}d"
  val r115 = r"^([^,]{1,},){3}d"
  val r116 = r"^([^a-z])|(\^)$$"
  val r117 = r"^([a]{1})*$$"
  val r118 = r"^([ab]*?)(b)?(c)$$"
  val r119 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
  val r120 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
  val r121 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
  val r122 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
  val r123 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"
  val r124 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"
  val r125 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
  val r126 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
  val r127 = r"^(a(b)?)+$$"
  val r128 = r"^(a)?a$$"
  val r129 = r"^(a+)*ax"
  val r130 = r"^(a\1?)(a\1?)(a\2?)(a\3?)$$"
  val r131 = r"^(a\1?){4}$$"
  val r132 = r"^(a\1?){4}$$"
  val r133 = r"^(aa(bb)?)+$$"
  val r134 = r"^(b+?|a){1,2}c"
  val r135 = r"^(b+?|a){1,2}c"
  val r140 = r"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
  val r141 = r"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
  val r142 = r"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"
  val r143 = r"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"
  val r144 = r"^.{2,3}?((?:b|a|r)+)\1$$"
  val r145 = r"^.{2,3}?((?:b|a|r)+?)\1$$"
  val r146 = r"^.{2,3}?(.+)\1$$"
  val r147 = r"^.{2,3}?(.+?)\1$$"
  val r148 = r"^.{3,4}((?:b|a|r)+)\1$$"
  val r149 = r"^.{3,4}((?:b|a|r)+?)\1$$"
  val r150 = r"^.{3,4}(.+)\1$$"
  val r151 = r"^.{3,4}(.+?)\1$$"
  val r152 = r"^m?(\d)(.*)\1$$"
  val r153 = r"^m?(\D)(.*)\1$$"
  val r154 = r"^m?(\S)(.*)\1$$"
  val r155 = r"^m?(\W)(.*)\1$$"
  val r156 = r"^m?(\w)(.*)\1$$"
  val r157 = r"a(?:b|(c|e){1,2}?|d)+?(.)"
  val r158 = r"a(?:b|c|d)(.)"
  val r159 = r"a(?:b|c|d)*(.)"
  val r160 = r"a(?:b|c|d)+(.)"
  val r161 = r"a(?:b|c|d)+?(.)"
  val r162 = r"a(?:b|c|d)+?(.)"
  val r163 = r"a(?:b|c|d){5,6}(.)"
  val r164 = r"a(?:b|c|d){5,6}?(.)"
  val r165 = r"a(?:b|c|d){5,7}(.)"
  val r166 = r"a(?:b|c|d){5,7}?(.)"
  val r167 = r"a(?:b|c|d){6,7}(.)"
  val r168 = r"a(?:b|c|d){6,7}?(.)"
  val r169 = r"a([bc]*)(c*d)"
  val r170 = r"a([bc]*)(c+d)"
  val r171 = r"a([bc]*)c*"
  val r172 = r"a([bc]+)(c*d)"
  val r173 = r"a(bc)d"
  val r174 = r"foo(aA)*+b"
  val r175 = r"foo(aA)++b"
  val r176 = r"foo(aA)?+b"
  val r177 = r"foo(aA){1,5}+b"
  val r178 = r"foo(aA|bB)*+b"
  val r179 = r"foo(aA|bB)++b"
  val r180 = r"foo(aA|bB)?+b"
  val r181 = r"foo(aA|bB){1,5}+b"
  val r182 = r"X(\w+)(?=\s)|X(\w+)"
  val r183 = r"x(~~)*(?:(?:F)?)?"

  @Benchmark
  def benchmark1 = r1.unapply("a")

  @Benchmark
  def benchmark2 = r2.unapply("aa")

  @Benchmark
  def benchmark3 = r3.unapply("a")

  @Benchmark
  def benchmark4 = r4.unapply("aaaacccc")

  @Benchmark
  def benchmark5 = r5.unapply("bbbbcccc")

  @Benchmark
  def benchmark6 = r6.unapply("ab")

  @Benchmark
  def benchmark7 = r7.unapply("Ab")

  @Benchmark
  def benchmark8 = r8.unapply("ab")

  @Benchmark
  def benchmark9 = r9.unapply("Ab")

  @Benchmark
  def benchmark10 = r10.unapply("ababbbcbc")

  @Benchmark
  def benchmark11 = r11.unapply("ababbbcbc")

  @Benchmark
  def benchmark12 = r12.unapply("abcd")

  @Benchmark
  def benchmark13 = r13.unapply("foobar")

  @Benchmark
  def benchmark14 = r14.unapply("abcde")

  @Benchmark
  def benchmark15 = r15.unapply("foobar")

  // it should "pass test 16: ([[:digit:]-[:alpha:]]+)" in pending // {
  //   val r16 = r"([[:digit:]-[:alpha:]]+)"
  //   "-" should matchPattern { case r16("-") => }
  // }

  // it should "pass test 17: ([[:digit:]-z]+)" in pending // {
  //   val r17 = r"([[:digit:]-z]+)"
  //   "-" should matchPattern { case r17("-") => }
  // }

  // TODO: Parser bug
  // it should "pass test 18: ([\\d-\\s]+)" in pending // {
  //   val r18 = r"([\\d-\\s]+)"
  //   "-" should matchPattern { case r18("-") => }
  // }

  // TODO: Parser bug
  // it should "pass test 19: ([\\d-z]+)" in pending // {
  //   val r19 = r"([\\d-z]+)"
  //   "-" should matchPattern { case r19("-") => }
  // }

  // TODO: Parser bug
  // it should "pass test 20: ([\\w:]+::)?(\\w+)$" in pending // {
  //   val r20 = r"([\w:]+::)?(\w+)$$"
  //   "abcd" should matchPattern { case r20(None, "abcd") => }
  // }

  // TODO: Parser bug
  // it should "pass test 21: ([\\w:]+::)?(\\w+)$" in pending // {
  //   val r21 = r"([\w:]+::)?(\w+)$$"
  //   "xy:z:::abcd" should matchPattern { case r21(Some("xy:z:::"), "abcd") => }
  // }

  @Benchmark
  def benchmark22 = r22.unapply("abcabc")

  @Benchmark
  def benchmark23 = r23.unapply("abcd")

  @Benchmark
  def benchmark24 = r24.unapply("abbbcd")

  @Benchmark
  def benchmark25 = r25.unapply("WORDS")

  @Benchmark
  def benchmark26 = r26.unapply("WORDS")

  @Benchmark
  def benchmark27 = r27.unapply("WORDS")

  @Benchmark
  def benchmark28 = r28.unapply("WORDS")

  @Benchmark
  def benchmark29 = r29.unapply("fool")

  @Benchmark
  def benchmark30 = r30.unapply("fool")

  @Benchmark
  def benchmark31 = r31.unapply("3.1415926")

  @Benchmark
  def benchmark32 = r32.unapply("one:")

  @Benchmark
  def benchmark33 = r33.unapply("ab")

  @Benchmark
  def benchmark34 = r34.unapply("a")

  @Benchmark
  def benchmark35 = r35.unapply("abc")

  @Benchmark
  def benchmark36 = r36.unapply("b")

  @Benchmark
  def benchmark37 = r37.unapply("a")

  @Benchmark
  def benchmark38 = r38.unapply("ab")

  @Benchmark
  def benchmark39 = r39.unapply("ab")

  @Benchmark
  def benchmark40 = r40.unapply("ab")

  @Benchmark
  def benchmark41 = r41.unapply("ab")

  @Benchmark
  def benchmark42 = r42.unapply("aAaAaAaAaAb")

  @Benchmark
  def benchmark43 = r43.unapply("aAaAaAaAaAb")

  @Benchmark
  def benchmark44 = r44.unapply("aAb")

  @Benchmark
  def benchmark45 = r45.unapply("aAaAaAaAaAb")

  @Benchmark
  def benchmark46 = r46.unapply("bBbBbBbBbBb")

  @Benchmark
  def benchmark47 = r47.unapply("aAbBaAaAbBb")

  @Benchmark
  def benchmark48 = r48.unapply("bBb")

  @Benchmark
  def benchmark49 = r49.unapply("bBaAbBaAbBb")

  @Benchmark
  def benchmark50 = r50.unapply("ab")

  @Benchmark
  def benchmark51 = r51.unapply("abc")

  @Benchmark
  def benchmark52 = r52.unapply("abcabc")

  @Benchmark
  def benchmark53 = r53.unapply("abc")

  @Benchmark
  def benchmark54 = r54.unapply("abc")

  @Benchmark
  def benchmark55 = r55.unapply("xyz")

  @Benchmark
  def benchmark56 = r56.unapply("xyz")

  @Benchmark
  def benchmark57 = r57.unapply("ef")

  @Benchmark
  def benchmark58 = r58.unapply("effgz")

  @Benchmark
  def benchmark59 = r59.unapply("ij")

  @Benchmark
  def benchmark60 = r60.unapply("foo1bar2baz3y")

  @Benchmark
  def benchmark61 = r61.unapply("foo1bar2baz3y")

  @Benchmark
  def benchmark62 = r62.unapply("fool")

  @Benchmark
  def benchmark63 = r63.unapply("fool")

  @Benchmark
  def benchmark64 = r64.unapply("fool")

  @Benchmark
  def benchmark65 = r65.unapply("fool")

  @Benchmark
  def benchmark66 = r66.unapply("xayxay")

  @Benchmark
  def benchmark67 = r67.unapply("xayxay")

  @Benchmark
  def benchmark68 = r68.unapply("zzzzzzzzzzzzzzzz-xayxayxayxayZ")

  @Benchmark
  def benchmark69 = r69.unapply("WORDS")

  @Benchmark
  def benchmark70 = r70.unapply("WORDS")

  @Benchmark
  def benchmark71 = r71.unapply("WORDS")

  @Benchmark
  def benchmark72 = r72.unapply("fool")

  @Benchmark
  def benchmark73 = r73.unapply("WORDS")

  @Benchmark
  def benchmark74 = r74.unapply("WORDS")

  @Benchmark
  def benchmark75 = r75.unapply("xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark76 = r76.unapply("xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark77 = r77.unapply("xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark78 = r78.unapply("xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark79 = r79.unapply("xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark80 = r80.unapply("xyzQzWlongishoverblownW")

  @Benchmark
  def benchmark81 = r81.unapply("abx")

  @Benchmark
  def benchmark82 = r82.unapply("2")

  @Benchmark
  def benchmark83 = r83.unapply("(a, b)")

  @Benchmark
  def benchmark84 = r84.unapply("aaaaX5")

  @Benchmark
  def benchmark85 = r85.unapply("aax")

  @Benchmark
  def benchmark86 = r86.unapply("aax")

  // it should "pass test 87: ^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$" in pending // {
  //   val r87 = r"^(.*?)\s*\|\s*(?:\/\s*|)'(.+)'$$"
  //   "text|\'sec\'" should matchPattern { case r87("text", "sec") => }
  // }

  @Benchmark
  def benchmark88 = r88.unapply("AB")

  @Benchmark
  def benchmark89 = r89.unapply("a,b,c")

  @Benchmark
  def benchmark90 = r90.unapply("x1")

  @Benchmark
  def benchmark91 = r91.unapply("1")

  @Benchmark
  def benchmark92 = r92.unapply("X12")

  @Benchmark
  def benchmark93 = r93.unapply("XX12")

  @Benchmark
  def benchmark94 = r94.unapply("foobarbar")

  @Benchmark
  def benchmark95 = r95.unapply("foobarbar")

  @Benchmark
  def benchmark96 = r96.unapply("foobarbar")

  @Benchmark
  def benchmark97 = r97.unapply("foobarbar")

  @Benchmark
  def benchmark98 = r98.unapply("foobarbar")

  @Benchmark
  def benchmark99 = r99.unapply("foobarbar")

  @Benchmark
  def benchmark100 = r100.unapply("foobarbar")

  @Benchmark
  def benchmark101 = r101.unapply("foobarbar")

  @Benchmark
  def benchmark102 = r102.unapply("012cxx0190")

  @Benchmark
  def benchmark103 = r103.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark104 = r104.unapply("a,b,c")

  @Benchmark
  def benchmark105 = r105.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark106 = r106.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark107 = r107.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark108 = r108.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark109 = r109.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark110 = r110.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark111 = r111.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark112 = r112.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark113 = r113.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark114 = r114.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark115 = r115.unapply("aaa,b,c,d")

  @Benchmark
  def benchmark116 = r116.unapply(".")

  @Benchmark
  def benchmark117 = r117.unapply("aa")

  @Benchmark
  def benchmark118 = r118.unapply("abac")

  @Benchmark
  def benchmark119 = r119.unapply("ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark120 = r120.unapply("ZEQQQX:")

  @Benchmark
  def benchmark121 = r121.unapply("ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark122 = r122.unapply("ZEQQQX:")

  @Benchmark
  def benchmark123 = r123.unapply("ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark124 = r124.unapply("ZEQQQX:")

  @Benchmark
  def benchmark125 = r125.unapply("ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark126 = r126.unapply("ZEQQQX:")

  @Benchmark
  def benchmark127 = r127.unapply("aba")

  @Benchmark
  def benchmark128 = r128.unapply("a")

  @Benchmark
  def benchmark129 = r129.unapply("aax")

  @Benchmark
  def benchmark130 = r130.unapply("aaaaaa")

  @Benchmark
  def benchmark131 = r131.unapply("aaaaaa")

  @Benchmark
  def benchmark132 = r132.unapply("aaaaaaaaaa")

  @Benchmark
  def benchmark133 = r133.unapply("aabbaa")

  @Benchmark
  def benchmark134 = r134.unapply("bbbac")

  @Benchmark
  def benchmark135 = r135.unapply("bbbbac")

  // it should "pass test 136: ^(foo|)bar$" in pending // {
  //   val r136 = r"^(foo|)bar$"
  //   "bar" should matchPattern { case r136("") => }
  // }

  // it should "pass test 137: ^(foo||baz)bar$" in pending // {
  //   val r137 = r"^(foo||baz)bar$"
  //   "bar" should matchPattern { case r137("") => }
  // }

  // it should "pass test 138: ^(foo||baz)bar$" in pending // {
  //   val r138 = r"^(foo||baz)bar$"
  //   "bazbar" should matchPattern { case r138("baz") => }
  // }

  // it should "pass test 139: ^(foo||baz)bar$" in pending // {
  //   val r139 = r"^(foo||baz)bar$"
  //   "foobar" should matchPattern { case r139("foo") => }
  // }

  @Benchmark
  def benchmark140 = r140.unapply("ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark141 = r141.unapply("ZEQQQX:")

  @Benchmark
  def benchmark142 = r142.unapply("ZEQQQQQQQQQQQQQQQQQQP:")

  @Benchmark
  def benchmark143 = r143.unapply("ZEQQQX:")

  @Benchmark
  def benchmark144 = r144.unapply("foobarbar")

  @Benchmark
  def benchmark145 = r145.unapply("foobarbar")

  @Benchmark
  def benchmark146 = r146.unapply("foobarbar")

  @Benchmark
  def benchmark147 = r147.unapply("foobarbar")

  @Benchmark
  def benchmark148 = r148.unapply("foobarbar")

  @Benchmark
  def benchmark149 = r149.unapply("foobarbar")

  @Benchmark
  def benchmark150 = r150.unapply("foobarbar")

  @Benchmark
  def benchmark151 = r151.unapply("foobarbar")

  @Benchmark
  def benchmark152 = r152.unapply("5b5")

  @Benchmark
  def benchmark153 = r153.unapply("aba")

  @Benchmark
  def benchmark154 = r154.unapply("aba")

  @Benchmark
  def benchmark155 = r155.unapply(":b:")

  @Benchmark
  def benchmark156 = r156.unapply("aba")

  @Benchmark
  def benchmark157 = r157.unapply("ace")

  @Benchmark
  def benchmark158 = r158.unapply("ace")

  @Benchmark
  def benchmark159 = r159.unapply("ace")

  @Benchmark
  def benchmark160 = r160.unapply("acdbcdbe")

  @Benchmark
  def benchmark161 = r161.unapply("acdbcdbe")

  @Benchmark
  def benchmark162 = r162.unapply("ace")

  @Benchmark
  def benchmark163 = r163.unapply("acdbcdbe")

  @Benchmark
  def benchmark164 = r164.unapply("acdbcdbe")

  @Benchmark
  def benchmark165 = r165.unapply("acdbcdbe")

  @Benchmark
  def benchmark166 = r166.unapply("acdbcdbe")

  @Benchmark
  def benchmark167 = r167.unapply("acdbcdbe")

  @Benchmark
  def benchmark168 = r168.unapply("acdbcdbe")

  @Benchmark
  def benchmark169 = r169.unapply("abcd")

  @Benchmark
  def benchmark170 = r170.unapply("abcd")

  @Benchmark
  def benchmark171 = r171.unapply("abc")

  @Benchmark
  def benchmark172 = r172.unapply("abcd")

  @Benchmark
  def benchmark173 = r173.unapply("abcd")

  @Benchmark
  def benchmark174 = r174.unapply("fooaAaAaAaAaAb")

  @Benchmark
  def benchmark175 = r175.unapply("fooaAaAaAaAaAb")

  @Benchmark
  def benchmark176 = r176.unapply("fooaAb")

  @Benchmark
  def benchmark177 = r177.unapply("fooaAaAaAaAaAb")

  @Benchmark
  def benchmark178 = r178.unapply("foobBbBaAaAaAb")

  @Benchmark
  def benchmark179 = r179.unapply("foobBaAbBaAbBb")

  @Benchmark
  def benchmark180 = r180.unapply("foobBb")

  @Benchmark
  def benchmark181 = r181.unapply("foobBaAaAaAaAb")

  @Benchmark
  def benchmark182 = r182.unapply("Xab")

  @Benchmark
  def benchmark183 = r183.unapply("x~~")
}
