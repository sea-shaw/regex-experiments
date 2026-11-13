package experiments.cps.macros

import experiments.cps.macros.regex.r
import experiments.cps.macros.utils.code
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.golden.GoldenMatchers
import org.scalatest.matchers.should.Matchers.should

class GoldenTests extends AnyFlatSpec with GoldenMatchers {
  val dir = "cps/src/test/resources/macros"

  behavior of "code"

  it should "match single character" in {
    code(r"a") should matchGolden (s"$dir/single-character.golden")
  }

  it should "match single capture" in {
    code(r"(a)") should matchGolden (s"$dir/single-capture.golden")
  }

  it should "match multiple characters" in {
    code(r"abc") should matchGolden (s"$dir/multiple-characters.golden")
  }

  it should "match multiple captures" in {
    code(r"(a)(b)(c)") should matchGolden (s"$dir/multiple-captures.golden")
  }

  it should "match nested captures" in {
    code(r"(a(b(c)))") should matchGolden (s"$dir/nested-captures.golden")
  }

  it should "match optional patterns" in {
    code(r"(a)?") should matchGolden (s"$dir/optional.golden")
  }

  it should "match alternative patterns" in {
    code(r"(a)|(b)") should matchGolden (s"$dir/alternative.golden")
  }

  it should "match join" in {
    code(r"(?:a|b)c") should matchGolden(s"$dir/join.golden")
  }

  it should "match multiple alternatives" in {
    code(r"(?:(a)|(b))(?:(c)|(d))(?:(e)|(f))") should matchGolden(s"$dir/multiple-alternatives.golden")
  }

  it should "match repeated patterns" in {
    code(r"a*") should matchGolden(s"$dir/repeated-pattern.golden")
  }

  it should "match repeated capture groups" in {
    code(r"(a)*") should matchGolden(s"$dir/repeated-capture-group.golden")
  }

  it should "match nested repetitions" in {
    code(r"(a(b)*)*") should matchGolden(s"$dir/nested-repetition.golden")
  }

  it should "match repeated optionals" in {
    code(r"(?:a?)*") should matchGolden(s"$dir/repeated-optional.golden")
  }

  it should "match repetition without nested backtracking" in {
    code(r"a*a") should matchGolden(s"$dir/tail-rec-repetition.golden")
  }
}
