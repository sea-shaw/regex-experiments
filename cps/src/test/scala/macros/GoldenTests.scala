package experiments.cps.macros

import experiments.cps.macros.regex.codeString
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.golden.GoldenMatchers
import org.scalatest.matchers.should.Matchers.should

class GoldenTests extends AnyFlatSpec with GoldenMatchers {
  val dir = "cps/src/test/resources/macros"

  behavior of "code"

  it should "match single character" in {
    codeString("a") should matchGolden (s"$dir/single-character.golden")
  }

  it should "match single capture" in {
    codeString("(a)") should matchGolden (s"$dir/single-capture.golden")
  }

  it should "match multiple characters" in {
    codeString("abc") should matchGolden (s"$dir/multiple-characters.golden")
  }

  it should "match multiple captures" in {
    codeString("(a)(b)(c)") should matchGolden (s"$dir/multiple-captures.golden")
  }

  it should "match nested captures" in {
    codeString("(a(b(c)))") should matchGolden (s"$dir/nested-captures.golden")
  }

  it should "match optional patterns" in {
    codeString("(a)?") should matchGolden (s"$dir/optional.golden")
  }

  it should "match alternative patterns" in {
    codeString("(a)|(b)") should matchGolden (s"$dir/alternative.golden")
  }

  it should "match join" in {
    codeString("(?:a|b)c") should matchGolden(s"$dir/join.golden")
  }

  it should "match multiple alternatives" in {
    codeString("(?:(a)|(b))(?:(c)|(d))(?:(e)|(f))") should matchGolden(s"$dir/multiple-alternatives.golden")
  }
}
