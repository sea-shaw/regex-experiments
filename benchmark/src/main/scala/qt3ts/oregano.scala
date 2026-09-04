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

  @Benchmark
  def benchmark1 = {
    val r1 = r"((((((((((a))))))))))"
    r1.unapply("a")
  }

  @Benchmark
  def benchmark2 = {
    val r2 = r"((((((((((a))))))))))\10"
    r2.unapply("aa")
  }

  @Benchmark
  def benchmark3 = {
    val r3 = r"(((((((((a)))))))))"
    r3.unapply("a")
  }

  @Benchmark
  def benchmark4 = {
    val r4 = r"((?:aaaa|bbbb)cccc)?"
    r4.unapply("aaaacccc")
  }

  @Benchmark
  def benchmark5 = {
    val r5 = r"((?:aaaa|bbbb)cccc)?"
    r5.unapply("bbbbcccc")
  }

  @Benchmark
  def benchmark6 = {
    val r6 = r"((?i)a)b"
    r6.unapply("ab")
  }

  @Benchmark
  def benchmark7 = {
    val r7 = r"((?i)a)b"
    r7.unapply("Ab")
  }

  @Benchmark
  def benchmark8 = {
    val r8 = r"((?i:a))b"
    r8.unapply("ab")
  }

  @Benchmark
  def benchmark9 = {
    val r9 = r"((?i:a))b"
    r9.unapply("Ab")
  }

  @Benchmark
  def benchmark10 = {
    val r10 = r"(([a-c])b*?\2)*"
    r10.unapply("ababbbcbc")
  }

  @Benchmark
  def benchmark11 = {
    val r11 = r"(([a-c])b*?\2){3}"
    r11.unapply("ababbbcbc")
  }

  @Benchmark
  def benchmark12 = {
    val r12 = r"((a)(b)c)(d)"
    r12.unapply("abcd")
  }

  @Benchmark
  def benchmark13 = {
    val r13 = r"((foo)|(bar))*"
    r13.unapply("foobar")
  }

  @Benchmark
  def benchmark14 = {
    val r14 = r"(.*)c(.*)"
    r14.unapply("abcde")
  }

  @Benchmark
  def benchmark15 = {
    val r15 = r"(?:(f)(o)(o)|(b)(a)(r))*"
    r15.unapply("foobar")
  }

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
  def benchmark22 = {
    val r22 = r"([a-c]*)\1"
    r22.unapply("abcabc")
  }

  @Benchmark
  def benchmark23 = {
    val r23 = r"([abc])*bcd"
    r23.unapply("abcd")
  }

  @Benchmark
  def benchmark24 = {
    val r24 = r"([abc])*d"
    r24.unapply("abbbcd")
  }

  @Benchmark
  def benchmark25 = {
    val r25 = r"([yX].|WORDS|[yX].|WORD)+S"
    r25.unapply("WORDS")
  }

  @Benchmark
  def benchmark26 = {
    val r26 = r"([yX].|WORDS|[yX].|WORD)S"
    r26.unapply("WORDS")
  }

  @Benchmark
  def benchmark27 = {
    val r27 = r"([yX].|WORDS|WORD|[xY].)+S"
    r27.unapply("WORDS")
  }

  @Benchmark
  def benchmark28 = {
    val r28 = r"([yX].|WORDS|WORD|[xY].)S"
    r28.unapply("WORDS")
  }

  @Benchmark
  def benchmark29 = {
    val r29 = r"([zx].|foo|fool|[zq].|money|parted|[yx].)$$"
    r29.unapply("fool")
  }

  @Benchmark
  def benchmark30 = {
    val r30 = r"([zx].|foo|fool|[zq].|money|parted|[yx].)+$$"
    r30.unapply("fool")
  }

  @Benchmark
  def benchmark31 = {
    val r31 = r"(\d+\.\d+)"
    r31.unapply("3.1415926")
  }

  @Benchmark
  def benchmark32 = {
    val r32 = r"(\w+:)+"
    r32.unapply("one:")
  }

  @Benchmark
  def benchmark33 = {
    val r33 = r"(^|a)b"
    r33.unapply("ab")
  }

  @Benchmark
  def benchmark34 = {
    val r34 = r"(a)?(a)+"
    r34.unapply("a")
  }

  @Benchmark
  def benchmark35 = {
    val r35 = r"(a)b(c)"
    r35.unapply("abc")
  }

  @Benchmark
  def benchmark36 = {
    val r36 = r"(a)|(b)"
    r36.unapply("b")
  }

  @Benchmark
  def benchmark37 = {
    val r37 = r"(a)|\1"
    r37.unapply("a")
  }

  @Benchmark
  def benchmark38 = {
    val r38 = r"(a+|b)*"
    r38.unapply("ab")
  }

  @Benchmark
  def benchmark39 = {
    val r39 = r"(a+|b)+"
    r39.unapply("ab")
  }

  @Benchmark
  def benchmark40 = {
    val r40 = r"(a+|b){0,}"
    r40.unapply("ab")
  }

  @Benchmark
  def benchmark41 = {
    val r41 = r"(a+|b){1,}"
    r41.unapply("ab")
  }

  @Benchmark
  def benchmark42 = {
    val r42 = r"(aA)*+b"
    r42.unapply("aAaAaAaAaAb")
  }

  @Benchmark
  def benchmark43 = {
    val r43 = r"(aA)++b"
    r43.unapply("aAaAaAaAaAb")
  }

  @Benchmark
  def benchmark44 = {
    val r44 = r"(aA)?+b"
    r44.unapply("aAb")
  }

  @Benchmark
  def benchmark45 = {
    val r45 = r"(aA){1,5}+b"
    r45.unapply("aAaAaAaAaAb")
  }

  @Benchmark
  def benchmark46 = {
    val r46 = r"(aA|bB)*+b"
    r46.unapply("bBbBbBbBbBb")
  }

  @Benchmark
  def benchmark47 = {
    val r47 = r"(aA|bB)++b"
    r47.unapply("aAbBaAaAbBb")
  }

  @Benchmark
  def benchmark48 = {
    val r48 = r"(aA|bB)?+b"
    r48.unapply("bBb")
  }

  @Benchmark
  def benchmark49 = {
    val r49 = r"(aA|bB){1,5}+b"
    r49.unapply("bBaAbBaAbBb")
  }

  @Benchmark
  def benchmark50 = {
    val r50 = r"(ab)?(ab)+"
    r50.unapply("ab")
  }

  @Benchmark
  def benchmark51 = {
    val r51 = r"(abc)?(abc)+"
    r51.unapply("abc")
  }

  @Benchmark
  def benchmark52 = {
    val r52 = r"(abc)\1"
    r52.unapply("abcabc")
  }

  @Benchmark
  def benchmark53 = {
    val r53 = r"(ab|a)b*c"
    r53.unapply("abc")
  }

  @Benchmark
  def benchmark54 = {
    val r54 = r"(ab|ab*)bc"
    r54.unapply("abc")
  }

  @Benchmark
  def benchmark55 = {
    val r55 = r"(a|(bc)){0,0}+xyz"
    r55.unapply("xyz")
  }

  @Benchmark
  def benchmark56 = {
    val r56 = r"(a|(bc)){0,0}?xyz"
    r56.unapply("xyz")
  }

  @Benchmark
  def benchmark57 = {
    val r57 = r"(a|b|c|d|e)f"
    r57.unapply("ef")
  }

  @Benchmark
  def benchmark58 = {
    val r58 = r"(bc+d$$|ef*g.|h?i(j|k))"
    r58.unapply("effgz")
  }

  @Benchmark
  def benchmark59 = {
    val r59 = r"(bc+d$$|ef*g.|h?i(j|k))"
    r59.unapply("ij")
  }

  @Benchmark
  def benchmark60 = {
    val r60 = r"(foo[1x]|bar[2x]|baz[3x])*y"
    r60.unapply("foo1bar2baz3y")
  }

  @Benchmark
  def benchmark61 = {
    val r61 = r"(foo[1x]|bar[2x]|baz[3x])+y"
    r61.unapply("foo1bar2baz3y")
  }

  @Benchmark
  def benchmark62 = {
    val r62 = r"(foo|fool|[zx].|money|parted)$$"
    r62.unapply("fool")
  }

  @Benchmark
  def benchmark63 = {
    val r63 = r"(foo|fool|[zx].|money|parted)+$$"
    r63.unapply("fool")
  }

  @Benchmark
  def benchmark64 = {
    val r64 = r"(foo|fool|money|parted)$$"
    r64.unapply("fool")
  }

  @Benchmark
  def benchmark65 = {
    val r65 = r"(foo|fool|x.|money|parted)$$"
    r65.unapply("fool")
  }

  @Benchmark
  def benchmark66 = {
    val r66 = r"(q1|.)*(q2|.)*(x(a|bc)*y){2,3}"
    r66.unapply("xayxay")
  }

  @Benchmark
  def benchmark67 = {
    val r67 = r"(q1|.)*(q2|.)*(x(a|bc)*y){2,}"
    r67.unapply("xayxay")
  }

  @Benchmark
  def benchmark68 = {
    val r68 = r"(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z"
    r68.unapply("zzzzzzzzzzzzzzzz-xayxayxayxayZ")
  }

  @Benchmark
  def benchmark69 = {
    val r69 = r"(WORDS|WORD)S"
    r69.unapply("WORDS")
  }

  @Benchmark
  def benchmark70 = {
    val r70 = r"(WORDS|WORLD|WORD)+S"
    r70.unapply("WORDS")
  }

  @Benchmark
  def benchmark71 = {
    val r71 = r"(WORDS|WORLD|WORD)S"
    r71.unapply("WORDS")
  }

  @Benchmark
  def benchmark72 = {
    val r72 = r"(x.|foo|fool|x.|money|parted|y.)$$"
    r72.unapply("fool")
  }

  @Benchmark
  def benchmark73 = {
    val r73 = r"(X.|WORDS|WORD|Y.)S"
    r73.unapply("WORDS")
  }

  @Benchmark
  def benchmark74 = {
    val r74 = r"(X.|WORDS|X.|WORD)S"
    r74.unapply("WORDS")
  }

  @Benchmark
  def benchmark75 = {
    val r75 = r"(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*"
    r75.unapply("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark76 = {
    val r76 = r"(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+"
    r76.unapply("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark77 = {
    val r77 = r"(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+"
    r77.unapply("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark78 = {
    val r78 = r"(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++"
    r78.unapply("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark79 = {
    val r79 = r"(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}"
    r79.unapply("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark80 = {
    val r80 = r"(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+"
    r80.unapply("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark81 = {
    val r81 = r".*?(?:(\w)|(\w))x"
    r81.unapply("abx")
  }

  @Benchmark
  def benchmark82 = {
    val r82 = r"2(]*)?$$\1"
    r82.unapply("2")
  }

  @Benchmark
  def benchmark83 = {
    val r83 = r"\((.*), (.*)\)"
    r83.unapply("(a, b)")
  }

  @Benchmark
  def benchmark84 = {
    val r84 = r"^((?:aa)*)(?:X+((?:\d+|-)(?:X+(.+))?))?$$"
    r84.unapply("aaaaX5")
  }

  @Benchmark
  def benchmark85 = {
    val r85 = r"^((a|b)+)*ax"
    r85.unapply("aax")
  }

  @Benchmark
  def benchmark86 = {
    val r86 = r"^((a|bc)+)*ax"
    r86.unapply("aax")
  }

  // it should "pass test 87: ^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$" in pending // {
  //   val r87 = r"^(.*?)\s*\|\s*(?:\/\s*|)'(.+)'$$"
  //   "text|\'sec\'" should matchPattern { case r87("text", "sec") => }
  // }

  @Benchmark
  def benchmark88 = {
    val r88 = r"^(.+)?B"
    r88.unapply("AB")
  }

  @Benchmark
  def benchmark89 = {
    val r89 = r"^(.,){2}c"
    r89.unapply("a,b,c")
  }

  @Benchmark
  def benchmark90 = {
    val r90 = r"^(0+)?(?:x(1))?"
    r90.unapply("x1")
  }

  @Benchmark
  def benchmark91 = {
    val r91 = r"^(?:(\d)x)?\d$$"
    r91.unapply("1")
  }

  @Benchmark
  def benchmark92 = {
    val r92 = r"^(?:(X)?(\d)|(X)?(\d\d))$$"
    r92.unapply("X12")
  }

  @Benchmark
  def benchmark93 = {
    val r93 = r"^(?:(XX)?(\d)|(XX)?(\d\d))$$"
    r93.unapply("XX12")
  }

  @Benchmark
  def benchmark94 = {
    val r94 = r"^(?:f|o|b){2,3}?((?:b|a|r)+)\1$$"
    r94.unapply("foobarbar")
  }

  @Benchmark
  def benchmark95 = {
    val r95 = r"^(?:f|o|b){2,3}?((?:b|a|r)+?)\1$$"
    r95.unapply("foobarbar")
  }

  @Benchmark
  def benchmark96 = {
    val r96 = r"^(?:f|o|b){2,3}?(.+)\1$$"
    r96.unapply("foobarbar")
  }

  @Benchmark
  def benchmark97 = {
    val r97 = r"^(?:f|o|b){2,3}?(.+?)\1$$"
    r97.unapply("foobarbar")
  }

  @Benchmark
  def benchmark98 = {
    val r98 = r"^(?:f|o|b){3,4}((?:b|a|r)+)\1$$"
    r98.unapply("foobarbar")
  }

  @Benchmark
  def benchmark99 = {
    val r99 = r"^(?:f|o|b){3,4}((?:b|a|r)+?)\1$$"
    r99.unapply("foobarbar")
  }

  @Benchmark
  def benchmark100 = {
    val r100 = r"^(?:f|o|b){3,4}(.+)\1$$"
    r100.unapply("foobarbar")
  }

  @Benchmark
  def benchmark101 = {
    val r101 = r"^(?:f|o|b){3,4}(.+?)\1$$"
    r101.unapply("foobarbar")
  }

  @Benchmark
  def benchmark102 = {
    val r102 = r"^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?"
    r102.unapply("012cxx0190")
  }

  @Benchmark
  def benchmark103 = {
    val r103 = r"^([^,]*,){0,3}d"
    r103.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark104 = {
    val r104 = r"^([^,]*,){2}c"
    r104.unapply("a,b,c")
  }

  @Benchmark
  def benchmark105 = {
    val r105 = r"^([^,]*,){3,}d"
    r105.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark106 = {
    val r106 = r"^([^,]*,){3}d"
    r106.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark107 = {
    val r107 = r"^([^,]{0,3},){0,3}d"
    r107.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark108 = {
    val r108 = r"^([^,]{0,3},){3,}d"
    r108.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark109 = {
    val r109 = r"^([^,]{0,3},){3}d"
    r109.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark110 = {
    val r110 = r"^([^,]{1,3},){0,3}d"
    r110.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark111 = {
    val r111 = r"^([^,]{1,3},){3,}d"
    r111.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark112 = {
    val r112 = r"^([^,]{1,3},){3}d"
    r112.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark113 = {
    val r113 = r"^([^,]{1,},){0,3}d"
    r113.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark114 = {
    val r114 = r"^([^,]{1,},){3,}d"
    r114.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark115 = {
    val r115 = r"^([^,]{1,},){3}d"
    r115.unapply("aaa,b,c,d")
  }

  @Benchmark
  def benchmark116 = {
    val r116 = r"^([^a-z])|(\^)$$"
    r116.unapply(".")
  }

  @Benchmark
  def benchmark117 = {
    val r117 = r"^([a]{1})*$$"
    r117.unapply("aa")
  }

  @Benchmark
  def benchmark118 = {
    val r118 = r"^([ab]*?)(b)?(c)$$"
    r118.unapply("abac")
  }

  @Benchmark
  def benchmark119 = {
    val r119 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
    r119.unapply("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark120 = {
    val r120 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
    r120.unapply("ZEQQQX:")
  }

  @Benchmark
  def benchmark121 = {
    val r121 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
    r121.unapply("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark122 = {
    val r122 = r"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
    r122.unapply("ZEQQQX:")
  }

  @Benchmark
  def benchmark123 = {
    val r123 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"
    r123.unapply("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark124 = {
    val r124 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"
    r124.unapply("ZEQQQX:")
  }

  @Benchmark
  def benchmark125 = {
    val r125 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
    r125.unapply("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark126 = {
    val r126 = r"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"
    r126.unapply("ZEQQQX:")
  }

  @Benchmark
  def benchmark127 = {
    val r127 = r"^(a(b)?)+$$"
    r127.unapply("aba")
  }

  @Benchmark
  def benchmark128 = {
    val r128 = r"^(a)?a$$"
    r128.unapply("a")
  }

  @Benchmark
  def benchmark129 = {
    val r129 = r"^(a+)*ax"
    r129.unapply("aax")
  }

  @Benchmark
  def benchmark130 = {
    val r130 = r"^(a\1?)(a\1?)(a\2?)(a\3?)$$"
    r130.unapply("aaaaaa")
  }

  @Benchmark
  def benchmark131 = {
    val r131 = r"^(a\1?){4}$$"
    r131.unapply("aaaaaa")
  }

  @Benchmark
  def benchmark132 = {
    val r132 = r"^(a\1?){4}$$"
    r132.unapply("aaaaaaaaaa")
  }

  @Benchmark
  def benchmark133 = {
    val r133 = r"^(aa(bb)?)+$$"
    r133.unapply("aabbaa")
  }

  @Benchmark
  def benchmark134 = {
    val r134 = r"^(b+?|a){1,2}c"
    r134.unapply("bbbac")
  }

  @Benchmark
  def benchmark135 = {
    val r135 = r"^(b+?|a){1,2}c"
    r135.unapply("bbbbac")
  }

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
  def benchmark140 = {
    val r140 = r"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
    r140.unapply("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark141 = {
    val r141 = r"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"
    r141.unapply("ZEQQQX:")
  }

  @Benchmark
  def benchmark142 = {
    val r142 = r"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"
    r142.unapply("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark143 = {
    val r143 = r"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"
    r143.unapply("ZEQQQX:")
  }

  @Benchmark
  def benchmark144 = {
    val r144 = r"^.{2,3}?((?:b|a|r)+)\1$$"
    r144.unapply("foobarbar")
  }

  @Benchmark
  def benchmark145 = {
    val r145 = r"^.{2,3}?((?:b|a|r)+?)\1$$"
    r145.unapply("foobarbar")
  }

  @Benchmark
  def benchmark146 = {
    val r146 = r"^.{2,3}?(.+)\1$$"
    r146.unapply("foobarbar")
  }

  @Benchmark
  def benchmark147 = {
    val r147 = r"^.{2,3}?(.+?)\1$$"
    r147.unapply("foobarbar")
  }

  @Benchmark
  def benchmark148 = {
    val r148 = r"^.{3,4}((?:b|a|r)+)\1$$"
    r148.unapply("foobarbar")
  }

  @Benchmark
  def benchmark149 = {
    val r149 = r"^.{3,4}((?:b|a|r)+?)\1$$"
    r149.unapply("foobarbar")
  }

  @Benchmark
  def benchmark150 = {
    val r150 = r"^.{3,4}(.+)\1$$"
    r150.unapply("foobarbar")
  }

  @Benchmark
  def benchmark151 = {
    val r151 = r"^.{3,4}(.+?)\1$$"
    r151.unapply("foobarbar")
  }

  @Benchmark
  def benchmark152 = {
    val r152 = r"^m?(\d)(.*)\1$$"
    r152.unapply("5b5")
  }

  @Benchmark
  def benchmark153 = {
    val r153 = r"^m?(\D)(.*)\1$$"
    r153.unapply("aba")
  }

  @Benchmark
  def benchmark154 = {
    val r154 = r"^m?(\S)(.*)\1$$"
    r154.unapply("aba")
  }

  @Benchmark
  def benchmark155 = {
    val r155 = r"^m?(\W)(.*)\1$$"
    r155.unapply(":b:")
  }

  @Benchmark
  def benchmark156 = {
    val r156 = r"^m?(\w)(.*)\1$$"
    r156.unapply("aba")
  }

  @Benchmark
  def benchmark157 = {
    val r157 = r"a(?:b|(c|e){1,2}?|d)+?(.)"
    r157.unapply("ace")
  }

  @Benchmark
  def benchmark158 = {
    val r158 = r"a(?:b|c|d)(.)"
    r158.unapply("ace")
  }

  @Benchmark
  def benchmark159 = {
    val r159 = r"a(?:b|c|d)*(.)"
    r159.unapply("ace")
  }

  @Benchmark
  def benchmark160 = {
    val r160 = r"a(?:b|c|d)+(.)"
    r160.unapply("acdbcdbe")
  }

  @Benchmark
  def benchmark161 = {
    val r161 = r"a(?:b|c|d)+?(.)"
    r161.unapply("acdbcdbe")
  }

  @Benchmark
  def benchmark162 = {
    val r162 = r"a(?:b|c|d)+?(.)"
    r162.unapply("ace")
  }

  @Benchmark
  def benchmark163 = {
    val r163 = r"a(?:b|c|d){5,6}(.)"
    r163.unapply("acdbcdbe")
  }

  @Benchmark
  def benchmark164 = {
    val r164 = r"a(?:b|c|d){5,6}?(.)"
    r164.unapply("acdbcdbe")
  }

  @Benchmark
  def benchmark165 = {
    val r165 = r"a(?:b|c|d){5,7}(.)"
    r165.unapply("acdbcdbe")
  }

  @Benchmark
  def benchmark166 = {
    val r166 = r"a(?:b|c|d){5,7}?(.)"
    r166.unapply("acdbcdbe")
  }

  @Benchmark
  def benchmark167 = {
    val r167 = r"a(?:b|c|d){6,7}(.)"
    r167.unapply("acdbcdbe")
  }

  @Benchmark
  def benchmark168 = {
    val r168 = r"a(?:b|c|d){6,7}?(.)"
    r168.unapply("acdbcdbe")
  }

  @Benchmark
  def benchmark169 = {
    val r169 = r"a([bc]*)(c*d)"
    r169.unapply("abcd")
  }

  @Benchmark
  def benchmark170 = {
    val r170 = r"a([bc]*)(c+d)"
    r170.unapply("abcd")
  }

  @Benchmark
  def benchmark171 = {
    val r171 = r"a([bc]*)c*"
    r171.unapply("abc")
  }

  @Benchmark
  def benchmark172 = {
    val r172 = r"a([bc]+)(c*d)"
    r172.unapply("abcd")
  }

  @Benchmark
  def benchmark173 = {
    val r173 = r"a(bc)d"
    r173.unapply("abcd")
  }

  @Benchmark
  def benchmark174 = {
    val r174 = r"foo(aA)*+b"
    r174.unapply("fooaAaAaAaAaAb")
  }

  @Benchmark
  def benchmark175 = {
    val r175 = r"foo(aA)++b"
    r175.unapply("fooaAaAaAaAaAb")
  }

  @Benchmark
  def benchmark176 = {
    val r176 = r"foo(aA)?+b"
    r176.unapply("fooaAb")
  }

  @Benchmark
  def benchmark177 = {
    val r177 = r"foo(aA){1,5}+b"
    r177.unapply("fooaAaAaAaAaAb")
  }

  @Benchmark
  def benchmark178 = {
    val r178 = r"foo(aA|bB)*+b"
    r178.unapply("foobBbBaAaAaAb")
  }

  @Benchmark
  def benchmark179 = {
    val r179 = r"foo(aA|bB)++b"
    r179.unapply("foobBaAbBaAbBb")
  }

  @Benchmark
  def benchmark180 = {
    val r180 = r"foo(aA|bB)?+b"
    r180.unapply("foobBb")
  }

  @Benchmark
  def benchmark181 = {
    val r181 = r"foo(aA|bB){1,5}+b"
    r181.unapply("foobBaAaAaAaAb")
  }

  @Benchmark
  def benchmark182 = {
    val r182 = r"X(\w+)(?=\s)|X(\w+)"
    r182.unapply("Xab")
  }

  @Benchmark
  def benchmark183 = {
    val r183 = r"x(~~)*(?:(?:F)?)?"
    r183.unapply("x~~")
  }
}
