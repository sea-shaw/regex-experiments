package experiments.benchmark.qt3ts

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Warmup(iterations = 8, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 8, time = 2, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ScalaQT3TSBenchmarks {

  @Benchmark
  def benchmark1 = {
    val r1 = raw"((((((((((a))))))))))".r
    r1.unapplySeq("a")
  }

  @Benchmark
  def benchmark2 = {
    val r2 = raw"((((((((((a))))))))))\10".r
    r2.unapplySeq("aa")
  }

  @Benchmark
  def benchmark3 = {
    val r3 = raw"(((((((((a)))))))))".r
    r3.unapplySeq("a")
  }

  @Benchmark
  def benchmark4 = {
    val r4 = raw"((?:aaaa|bbbb)cccc)?".r
    r4.unapplySeq("aaaacccc")
  }

  @Benchmark
  def benchmark5 = {
    val r5 = raw"((?:aaaa|bbbb)cccc)?".r
    r5.unapplySeq("bbbbcccc")
  }

  @Benchmark
  def benchmark6 = {
    val r6 = raw"((?i)a)b".r
    r6.unapplySeq("ab")
  }

  @Benchmark
  def benchmark7 = {
    val r7 = raw"((?i)a)b".r
    r7.unapplySeq("Ab")
  }

  @Benchmark
  def benchmark8 = {
    val r8 = raw"((?i:a))b".r
    r8.unapplySeq("ab")
  }

  @Benchmark
  def benchmark9 = {
    val r9 = raw"((?i:a))b".r
    r9.unapplySeq("Ab")
  }

  @Benchmark
  def benchmark10 = {
    val r10 = raw"(([a-c])b*?\2)*".r
    r10.unapplySeq("ababbbcbc")
  }

  @Benchmark
  def benchmark11 = {
    val r11 = raw"(([a-c])b*?\2){3}".r
    r11.unapplySeq("ababbbcbc")
  }

  @Benchmark
  def benchmark12 = {
    val r12 = raw"((a)(b)c)(d)".r
    r12.unapplySeq("abcd")
  }

  @Benchmark
  def benchmark13 = {
    val r13 = raw"((foo)|(bar))*".r
    r13.unapplySeq("foobar")
  }

  @Benchmark
  def benchmark14 = {
    val r14 = raw"(.*)c(.*)".r
    r14.unapplySeq("abcde")
  }

  @Benchmark
  def benchmark15 = {
    val r15 = raw"(?:(f)(o)(o)|(b)(a)(r))*".r
    r15.unapplySeq("foobar")
  }

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
  def benchmark22 = {
    val r22 = raw"([a-c]*)\1".r
    r22.unapplySeq("abcabc")
  }

  @Benchmark
  def benchmark23 = {
    val r23 = raw"([abc])*bcd".r
    r23.unapplySeq("abcd")
  }

  @Benchmark
  def benchmark24 = {
    val r24 = raw"([abc])*d".r
    r24.unapplySeq("abbbcd")
  }

  @Benchmark
  def benchmark25 = {
    val r25 = raw"([yX].|WORDS|[yX].|WORD)+S".r
    r25.unapplySeq("WORDS")
  }

  @Benchmark
  def benchmark26 = {
    val r26 = raw"([yX].|WORDS|[yX].|WORD)S".r
    r26.unapplySeq("WORDS")
  }

  @Benchmark
  def benchmark27 = {
    val r27 = raw"([yX].|WORDS|WORD|[xY].)+S".r
    r27.unapplySeq("WORDS")
  }

  @Benchmark
  def benchmark28 = {
    val r28 = raw"([yX].|WORDS|WORD|[xY].)S".r
    r28.unapplySeq("WORDS")
  }

  @Benchmark
  def benchmark29 = {
    val r29 = raw"([zx].|foo|fool|[zq].|money|parted|[yx].)$$".r
    r29.unapplySeq("fool")
  }

  @Benchmark
  def benchmark30 = {
    val r30 = raw"([zx].|foo|fool|[zq].|money|parted|[yx].)+$$".r
    r30.unapplySeq("fool")
  }

  @Benchmark
  def benchmark31 = {
    val r31 = raw"(\d+\.\d+)".r
    r31.unapplySeq("3.1415926")
  }

  @Benchmark
  def benchmark32 = {
    val r32 = raw"(\w+:)+".r
    r32.unapplySeq("one:")
  }

  @Benchmark
  def benchmark33 = {
    val r33 = raw"(^|a)b".r
    r33.unapplySeq("ab")
  }

  @Benchmark
  def benchmark34 = {
    val r34 = raw"(a)?(a)+".r
    r34.unapplySeq("a")
  }

  @Benchmark
  def benchmark35 = {
    val r35 = raw"(a)b(c)".r
    r35.unapplySeq("abc")
  }

  @Benchmark
  def benchmark36 = {
    val r36 = raw"(a)|(b)".r
    r36.unapplySeq("b")
  }

  @Benchmark
  def benchmark37 = {
    val r37 = raw"(a)|\1".r
    r37.unapplySeq("a")
  }

  @Benchmark
  def benchmark38 = {
    val r38 = raw"(a+|b)*".r
    r38.unapplySeq("ab")
  }

  @Benchmark
  def benchmark39 = {
    val r39 = raw"(a+|b)+".r
    r39.unapplySeq("ab")
  }

  @Benchmark
  def benchmark40 = {
    val r40 = raw"(a+|b){0,}".r
    r40.unapplySeq("ab")
  }

  @Benchmark
  def benchmark41 = {
    val r41 = raw"(a+|b){1,}".r
    r41.unapplySeq("ab")
  }

  @Benchmark
  def benchmark42 = {
    val r42 = raw"(aA)*+b".r
    r42.unapplySeq("aAaAaAaAaAb")
  }

  @Benchmark
  def benchmark43 = {
    val r43 = raw"(aA)++b".r
    r43.unapplySeq("aAaAaAaAaAb")
  }

  @Benchmark
  def benchmark44 = {
    val r44 = raw"(aA)?+b".r
    r44.unapplySeq("aAb")
  }

  @Benchmark
  def benchmark45 = {
    val r45 = raw"(aA){1,5}+b".r
    r45.unapplySeq("aAaAaAaAaAb")
  }

  @Benchmark
  def benchmark46 = {
    val r46 = raw"(aA|bB)*+b".r
    r46.unapplySeq("bBbBbBbBbBb")
  }

  @Benchmark
  def benchmark47 = {
    val r47 = raw"(aA|bB)++b".r
    r47.unapplySeq("aAbBaAaAbBb")
  }

  @Benchmark
  def benchmark48 = {
    val r48 = raw"(aA|bB)?+b".r
    r48.unapplySeq("bBb")
  }

  @Benchmark
  def benchmark49 = {
    val r49 = raw"(aA|bB){1,5}+b".r
    r49.unapplySeq("bBaAbBaAbBb")
  }

  @Benchmark
  def benchmark50 = {
    val r50 = raw"(ab)?(ab)+".r
    r50.unapplySeq("ab")
  }

  @Benchmark
  def benchmark51 = {
    val r51 = raw"(abc)?(abc)+".r
    r51.unapplySeq("abc")
  }

  @Benchmark
  def benchmark52 = {
    val r52 = raw"(abc)\1".r
    r52.unapplySeq("abcabc")
  }

  @Benchmark
  def benchmark53 = {
    val r53 = raw"(ab|a)b*c".r
    r53.unapplySeq("abc")
  }

  @Benchmark
  def benchmark54 = {
    val r54 = raw"(ab|ab*)bc".r
    r54.unapplySeq("abc")
  }

  @Benchmark
  def benchmark55 = {
    val r55 = raw"(a|(bc)){0,0}+xyz".r
    r55.unapplySeq("xyz")
  }

  @Benchmark
  def benchmark56 = {
    val r56 = raw"(a|(bc)){0,0}?xyz".r
    r56.unapplySeq("xyz")
  }

  @Benchmark
  def benchmark57 = {
    val r57 = raw"(a|b|c|d|e)f".r
    r57.unapplySeq("ef")
  }

  @Benchmark
  def benchmark58 = {
    val r58 = raw"(bc+d$$|ef*g.|h?i(j|k))".r
    r58.unapplySeq("effgz")
  }

  @Benchmark
  def benchmark59 = {
    val r59 = raw"(bc+d$$|ef*g.|h?i(j|k))".r
    r59.unapplySeq("ij")
  }

  @Benchmark
  def benchmark60 = {
    val r60 = raw"(foo[1x]|bar[2x]|baz[3x])*y".r
    r60.unapplySeq("foo1bar2baz3y")
  }

  @Benchmark
  def benchmark61 = {
    val r61 = raw"(foo[1x]|bar[2x]|baz[3x])+y".r
    r61.unapplySeq("foo1bar2baz3y")
  }

  @Benchmark
  def benchmark62 = {
    val r62 = raw"(foo|fool|[zx].|money|parted)$$".r
    r62.unapplySeq("fool")
  }

  @Benchmark
  def benchmark63 = {
    val r63 = raw"(foo|fool|[zx].|money|parted)+$$".r
    r63.unapplySeq("fool")
  }

  @Benchmark
  def benchmark64 = {
    val r64 = raw"(foo|fool|money|parted)$$".r
    r64.unapplySeq("fool")
  }

  @Benchmark
  def benchmark65 = {
    val r65 = raw"(foo|fool|x.|money|parted)$$".r
    r65.unapplySeq("fool")
  }

  @Benchmark
  def benchmark66 = {
    val r66 = raw"(q1|.)*(q2|.)*(x(a|bc)*y){2,3}".r
    r66.unapplySeq("xayxay")
  }

  @Benchmark
  def benchmark67 = {
    val r67 = raw"(q1|.)*(q2|.)*(x(a|bc)*y){2,}".r
    r67.unapplySeq("xayxay")
  }

  @Benchmark
  def benchmark68 = {
    val r68 = raw"(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z".r
    r68.unapplySeq("zzzzzzzzzzzzzzzz-xayxayxayxayZ")
  }

  @Benchmark
  def benchmark69 = {
    val r69 = raw"(WORDS|WORD)S".r
    r69.unapplySeq("WORDS")
  }

  @Benchmark
  def benchmark70 = {
    val r70 = raw"(WORDS|WORLD|WORD)+S".r
    r70.unapplySeq("WORDS")
  }

  @Benchmark
  def benchmark71 = {
    val r71 = raw"(WORDS|WORLD|WORD)S".r
    r71.unapplySeq("WORDS")
  }

  @Benchmark
  def benchmark72 = {
    val r72 = raw"(x.|foo|fool|x.|money|parted|y.)$$".r
    r72.unapplySeq("fool")
  }

  @Benchmark
  def benchmark73 = {
    val r73 = raw"(X.|WORDS|WORD|Y.)S".r
    r73.unapplySeq("WORDS")
  }

  @Benchmark
  def benchmark74 = {
    val r74 = raw"(X.|WORDS|X.|WORD)S".r
    r74.unapplySeq("WORDS")
  }

  @Benchmark
  def benchmark75 = {
    val r75 = raw"(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*".r
    r75.unapplySeq("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark76 = {
    val r76 = raw"(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+".r
    r76.unapplySeq("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark77 = {
    val r77 = raw"(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+".r
    r77.unapplySeq("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark78 = {
    val r78 = raw"(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++".r
    r78.unapplySeq("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark79 = {
    val r79 = raw"(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}".r
    r79.unapplySeq("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark80 = {
    val r80 = raw"(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+".r
    r80.unapplySeq("xyzQzWlongishoverblownW")
  }

  @Benchmark
  def benchmark81 = {
    val r81 = raw".*?(?:(\w)|(\w))x".r
    r81.unapplySeq("abx")
  }

  @Benchmark
  def benchmark82 = {
    val r82 = raw"2(]*)?$$\1".r
    r82.unapplySeq("2")
  }

  @Benchmark
  def benchmark83 = {
    val r83 = raw"\((.*), (.*)\)".r
    r83.unapplySeq("(a, b)")
  }

  @Benchmark
  def benchmark84 = {
    val r84 = raw"^((?:aa)*)(?:X+((?:\d+|-)(?:X+(.+))?))?$$".r
    r84.unapplySeq("aaaaX5")
  }

  @Benchmark
  def benchmark85 = {
    val r85 = raw"^((a|b)+)*ax".r
    r85.unapplySeq("aax")
  }

  @Benchmark
  def benchmark86 = {
    val r86 = raw"^((a|bc)+)*ax".r
    r86.unapplySeq("aax")
  }

  // it should "pass test 87: ^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$" in pending // {
  //   val r87 = raw"^(.*?)\s*\|\s*(?:\/\s*|)'(.+)'$$".r
  //   "text|\'sec\'" should matchPattern { case r87("text", "sec") => }
  // }

  @Benchmark
  def benchmark88 = {
    val r88 = raw"^(.+)?B".r
    r88.unapplySeq("AB")
  }

  @Benchmark
  def benchmark89 = {
    val r89 = raw"^(.,){2}c".r
    r89.unapplySeq("a,b,c")
  }

  @Benchmark
  def benchmark90 = {
    val r90 = raw"^(0+)?(?:x(1))?".r
    r90.unapplySeq("x1")
  }

  @Benchmark
  def benchmark91 = {
    val r91 = raw"^(?:(\d)x)?\d$$".r
    r91.unapplySeq("1")
  }

  @Benchmark
  def benchmark92 = {
    val r92 = raw"^(?:(X)?(\d)|(X)?(\d\d))$$".r
    r92.unapplySeq("X12")
  }

  @Benchmark
  def benchmark93 = {
    val r93 = raw"^(?:(XX)?(\d)|(XX)?(\d\d))$$".r
    r93.unapplySeq("XX12")
  }

  @Benchmark
  def benchmark94 = {
    val r94 = raw"^(?:f|o|b){2,3}?((?:b|a|r)+)\1$$".r
    r94.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark95 = {
    val r95 = raw"^(?:f|o|b){2,3}?((?:b|a|r)+?)\1$$".r
    r95.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark96 = {
    val r96 = raw"^(?:f|o|b){2,3}?(.+)\1$$".r
    r96.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark97 = {
    val r97 = raw"^(?:f|o|b){2,3}?(.+?)\1$$".r
    r97.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark98 = {
    val r98 = raw"^(?:f|o|b){3,4}((?:b|a|r)+)\1$$".r
    r98.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark99 = {
    val r99 = raw"^(?:f|o|b){3,4}((?:b|a|r)+?)\1$$".r
    r99.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark100 = {
    val r100 = raw"^(?:f|o|b){3,4}(.+)\1$$".r
    r100.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark101 = {
    val r101 = raw"^(?:f|o|b){3,4}(.+?)\1$$".r
    r101.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark102 = {
    val r102 = raw"^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?".r
    r102.unapplySeq("012cxx0190")
  }

  @Benchmark
  def benchmark103 = {
    val r103 = raw"^([^,]*,){0,3}d".r
    r103.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark104 = {
    val r104 = raw"^([^,]*,){2}c".r
    r104.unapplySeq("a,b,c")
  }

  @Benchmark
  def benchmark105 = {
    val r105 = raw"^([^,]*,){3,}d".r
    r105.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark106 = {
    val r106 = raw"^([^,]*,){3}d".r
    r106.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark107 = {
    val r107 = raw"^([^,]{0,3},){0,3}d".r
    r107.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark108 = {
    val r108 = raw"^([^,]{0,3},){3,}d".r
    r108.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark109 = {
    val r109 = raw"^([^,]{0,3},){3}d".r
    r109.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark110 = {
    val r110 = raw"^([^,]{1,3},){0,3}d".r
    r110.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark111 = {
    val r111 = raw"^([^,]{1,3},){3,}d".r
    r111.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark112 = {
    val r112 = raw"^([^,]{1,3},){3}d".r
    r112.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark113 = {
    val r113 = raw"^([^,]{1,},){0,3}d".r
    r113.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark114 = {
    val r114 = raw"^([^,]{1,},){3,}d".r
    r114.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark115 = {
    val r115 = raw"^([^,]{1,},){3}d".r
    r115.unapplySeq("aaa,b,c,d")
  }

  @Benchmark
  def benchmark116 = {
    val r116 = raw"^([^a-z])|(\^)$$".r
    r116.unapplySeq(".")
  }

  @Benchmark
  def benchmark117 = {
    val r117 = raw"^([a]{1})*$$".r
    r117.unapplySeq("aa")
  }

  @Benchmark
  def benchmark118 = {
    val r118 = raw"^([ab]*?)(b)?(c)$$".r
    r118.unapplySeq("abac")
  }

  @Benchmark
  def benchmark119 = {
    val r119 = raw"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):".r
    r119.unapplySeq("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark120 = {
    val r120 = raw"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):".r
    r120.unapplySeq("ZEQQQX:")
  }

  @Benchmark
  def benchmark121 = {
    val r121 = raw"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):".r
    r121.unapplySeq("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark122 = {
    val r122 = raw"^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):".r
    r122.unapplySeq("ZEQQQX:")
  }

  @Benchmark
  def benchmark123 = {
    val r123 = raw"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):".r
    r123.unapplySeq("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark124 = {
    val r124 = raw"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):".r
    r124.unapplySeq("ZEQQQX:")
  }

  @Benchmark
  def benchmark125 = {
    val r125 = raw"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):".r
    r125.unapplySeq("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark126 = {
    val r126 = raw"^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):".r
    r126.unapplySeq("ZEQQQX:")
  }

  @Benchmark
  def benchmark127 = {
    val r127 = raw"^(a(b)?)+$$".r
    r127.unapplySeq("aba")
  }

  @Benchmark
  def benchmark128 = {
    val r128 = raw"^(a)?a$$".r
    r128.unapplySeq("a")
  }

  @Benchmark
  def benchmark129 = {
    val r129 = raw"^(a+)*ax".r
    r129.unapplySeq("aax")
  }

  @Benchmark
  def benchmark130 = {
    val r130 = raw"^(a\1?)(a\1?)(a\2?)(a\3?)$$".r
    r130.unapplySeq("aaaaaa")
  }

  @Benchmark
  def benchmark131 = {
    val r131 = raw"^(a\1?){4}$$".r
    r131.unapplySeq("aaaaaa")
  }

  @Benchmark
  def benchmark132 = {
    val r132 = raw"^(a\1?){4}$$".r
    r132.unapplySeq("aaaaaaaaaa")
  }

  @Benchmark
  def benchmark133 = {
    val r133 = raw"^(aa(bb)?)+$$".r
    r133.unapplySeq("aabbaa")
  }

  @Benchmark
  def benchmark134 = {
    val r134 = raw"^(b+?|a){1,2}c".r
    r134.unapplySeq("bbbac")
  }

  @Benchmark
  def benchmark135 = {
    val r135 = raw"^(b+?|a){1,2}c".r
    r135.unapplySeq("bbbbac")
  }

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
  def benchmark140 = {
    val r140 = raw"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):".r
    r140.unapplySeq("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark141 = {
    val r141 = raw"^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):".r
    r141.unapplySeq("ZEQQQX:")
  }

  @Benchmark
  def benchmark142 = {
    val r142 = raw"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):".r
    r142.unapplySeq("ZEQQQQQQQQQQQQQQQQQQP:")
  }

  @Benchmark
  def benchmark143 = {
    val r143 = raw"^(XXX|YYY|Z.Q*X|Z[TE]Q*P):".r
    r143.unapplySeq("ZEQQQX:")
  }

  @Benchmark
  def benchmark144 = {
    val r144 = raw"^.{2,3}?((?:b|a|r)+)\1$$".r
    r144.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark145 = {
    val r145 = raw"^.{2,3}?((?:b|a|r)+?)\1$$".r
    r145.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark146 = {
    val r146 = raw"^.{2,3}?(.+)\1$$".r
    r146.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark147 = {
    val r147 = raw"^.{2,3}?(.+?)\1$$".r
    r147.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark148 = {
    val r148 = raw"^.{3,4}((?:b|a|r)+)\1$$".r
    r148.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark149 = {
    val r149 = raw"^.{3,4}((?:b|a|r)+?)\1$$".r
    r149.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark150 = {
    val r150 = raw"^.{3,4}(.+)\1$$".r
    r150.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark151 = {
    val r151 = raw"^.{3,4}(.+?)\1$$".r
    r151.unapplySeq("foobarbar")
  }

  @Benchmark
  def benchmark152 = {
    val r152 = raw"^m?(\d)(.*)\1$$".r
    r152.unapplySeq("5b5")
  }

  @Benchmark
  def benchmark153 = {
    val r153 = raw"^m?(\D)(.*)\1$$".r
    r153.unapplySeq("aba")
  }

  @Benchmark
  def benchmark154 = {
    val r154 = raw"^m?(\S)(.*)\1$$".r
    r154.unapplySeq("aba")
  }

  @Benchmark
  def benchmark155 = {
    val r155 = raw"^m?(\W)(.*)\1$$".r
    r155.unapplySeq(":b:")
  }

  @Benchmark
  def benchmark156 = {
    val r156 = raw"^m?(\w)(.*)\1$$".r
    r156.unapplySeq("aba")
  }

  @Benchmark
  def benchmark157 = {
    val r157 = raw"a(?:b|(c|e){1,2}?|d)+?(.)".r
    r157.unapplySeq("ace")
  }

  @Benchmark
  def benchmark158 = {
    val r158 = raw"a(?:b|c|d)(.)".r
    r158.unapplySeq("ace")
  }

  @Benchmark
  def benchmark159 = {
    val r159 = raw"a(?:b|c|d)*(.)".r
    r159.unapplySeq("ace")
  }

  @Benchmark
  def benchmark160 = {
    val r160 = raw"a(?:b|c|d)+(.)".r
    r160.unapplySeq("acdbcdbe")
  }

  @Benchmark
  def benchmark161 = {
    val r161 = raw"a(?:b|c|d)+?(.)".r
    r161.unapplySeq("acdbcdbe")
  }

  @Benchmark
  def benchmark162 = {
    val r162 = raw"a(?:b|c|d)+?(.)".r
    r162.unapplySeq("ace")
  }

  @Benchmark
  def benchmark163 = {
    val r163 = raw"a(?:b|c|d){5,6}(.)".r
    r163.unapplySeq("acdbcdbe")
  }

  @Benchmark
  def benchmark164 = {
    val r164 = raw"a(?:b|c|d){5,6}?(.)".r
    r164.unapplySeq("acdbcdbe")
  }

  @Benchmark
  def benchmark165 = {
    val r165 = raw"a(?:b|c|d){5,7}(.)".r
    r165.unapplySeq("acdbcdbe")
  }

  @Benchmark
  def benchmark166 = {
    val r166 = raw"a(?:b|c|d){5,7}?(.)".r
    r166.unapplySeq("acdbcdbe")
  }

  @Benchmark
  def benchmark167 = {
    val r167 = raw"a(?:b|c|d){6,7}(.)".r
    r167.unapplySeq("acdbcdbe")
  }

  @Benchmark
  def benchmark168 = {
    val r168 = raw"a(?:b|c|d){6,7}?(.)".r
    r168.unapplySeq("acdbcdbe")
  }

  @Benchmark
  def benchmark169 = {
    val r169 = raw"a([bc]*)(c*d)".r
    r169.unapplySeq("abcd")
  }

  @Benchmark
  def benchmark170 = {
    val r170 = raw"a([bc]*)(c+d)".r
    r170.unapplySeq("abcd")
  }

  @Benchmark
  def benchmark171 = {
    val r171 = raw"a([bc]*)c*".r
    r171.unapplySeq("abc")
  }

  @Benchmark
  def benchmark172 = {
    val r172 = raw"a([bc]+)(c*d)".r
    r172.unapplySeq("abcd")
  }

  @Benchmark
  def benchmark173 = {
    val r173 = raw"a(bc)d".r
    r173.unapplySeq("abcd")
  }

  @Benchmark
  def benchmark174 = {
    val r174 = raw"foo(aA)*+b".r
    r174.unapplySeq("fooaAaAaAaAaAb")
  }

  @Benchmark
  def benchmark175 = {
    val r175 = raw"foo(aA)++b".r
    r175.unapplySeq("fooaAaAaAaAaAb")
  }

  @Benchmark
  def benchmark176 = {
    val r176 = raw"foo(aA)?+b".r
    r176.unapplySeq("fooaAb")
  }

  @Benchmark
  def benchmark177 = {
    val r177 = raw"foo(aA){1,5}+b".r
    r177.unapplySeq("fooaAaAaAaAaAb")
  }

  @Benchmark
  def benchmark178 = {
    val r178 = raw"foo(aA|bB)*+b".r
    r178.unapplySeq("foobBbBaAaAaAb")
  }

  @Benchmark
  def benchmark179 = {
    val r179 = raw"foo(aA|bB)++b".r
    r179.unapplySeq("foobBaAbBaAbBb")
  }

  @Benchmark
  def benchmark180 = {
    val r180 = raw"foo(aA|bB)?+b".r
    r180.unapplySeq("foobBb")
  }

  @Benchmark
  def benchmark181 = {
    val r181 = raw"foo(aA|bB){1,5}+b".r
    r181.unapplySeq("foobBaAaAaAaAb")
  }

  @Benchmark
  def benchmark182 = {
    val r182 = raw"X(\w+)(?=\s)|X(\w+)".r
    r182.unapplySeq("Xab")
  }

  @Benchmark
  def benchmark183 = {
    val r183 = raw"x(~~)*(?:(?:F)?)?".r
    r183.unapplySeq("x~~")
  }
}
