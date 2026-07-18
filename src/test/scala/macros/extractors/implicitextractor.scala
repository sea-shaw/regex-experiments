package experiments.macros.extractors

import experiments.macros.extractors.implicitextractor.Alt
import experiments.macros.regex.Regex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class ImplicitExtractorUnitTests extends AnyFlatSpec {
  behavior of "Implicit extractor"

  it should "extract from alternatives of single capture groups" in {
    val r = Regex("(a)|(b)|(c)|(d)|(e)|(f)|(g)|(h)")
    "a" should matchPattern { case r(Alt(0, "a")) => }
    "b" should matchPattern { case r(Alt(1, "b")) => }
    "c" should matchPattern { case r(Alt(2, "c")) => }
    "d" should matchPattern { case r(Alt(3, "d")) => }
    "e" should matchPattern { case r(Alt(4, "e")) => }
    "f" should matchPattern { case r(Alt(5, "f")) => }
    "g" should matchPattern { case r(Alt(6, "g")) => }
    "h" should matchPattern { case r(Alt(7, "h")) => }
  }

  it should "extract from alternatives of multiple capture groups" in {
    val r = Regex("(a)(b)?|(c)(d)?|(e)(f)?")
    "a" should matchPattern { case r(Alt(0, ("a", None))) => }
    "ab" should matchPattern { case r(Alt(0, ("a", Some("b")))) => }
    "c" should matchPattern { case r(Alt(1, ("c", None))) => }
    "cd" should matchPattern { case r(Alt(1, ("c", Some("d")))) => }
    "e" should matchPattern { case r(Alt(2, ("e", None))) => }
    "ef" should matchPattern { case r(Alt(2, ("e", Some("f")))) => }
  }

  it should "extract from alternatives of alternatives of different types" in {
    val r = Regex("(?:(a)|(b)(c))|(?:(d)|(e)(f))")
    "a" should matchPattern { case r(Alt(0, Left("a"))) => }
    "bc" should matchPattern { case r(Alt(0, Right("b", "c"))) => }
    "d" should matchPattern { case r(Alt(1, Left("d"))) => }
    "ef" should matchPattern { case r(Alt(1, Right("e", "f"))) => }
  }
}
