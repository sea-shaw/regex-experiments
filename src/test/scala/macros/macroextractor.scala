package experiments.macros

import experiments.macros.macroextractor.Alt
import experiments.macros.regex.Regex
import org.scalatest.flatspec.AnyFlatSpec

class MacroExtractorUnitTests extends AnyFlatSpec {
  behavior of "Macro extractor"

  // Something about the inlining of `should matchPattern` and the `Alt.unapply`
  // macro causes a tree invariant assertion to fail, so use this here instead.
  extension [A] (a: A) {
    private infix def shouldMatchPattern(pat: PartialFunction[A, Unit]) = a match {
      case pat(()) => succeed
      case _       => fail()
    }
  }

  it should "extract from alternatives of single capture groups" in {
    val r = Regex("(a)|(b)|(c)|(d)|(e)|(f)|(g)|(h)")
    "a" shouldMatchPattern { case r(Alt(0, "a")) => }
    "b" shouldMatchPattern { case r(Alt(1, "b")) => }
    "c" shouldMatchPattern { case r(Alt(2, "c")) => }
    "d" shouldMatchPattern { case r(Alt(3, "d")) => }
    "e" shouldMatchPattern { case r(Alt(4, "e")) => }
    "f" shouldMatchPattern { case r(Alt(5, "f")) => }
    "g" shouldMatchPattern { case r(Alt(6, "g")) => }
    "h" shouldMatchPattern { case r(Alt(7, "h")) => }
  }

  it should "extract from alternatives of multiple capture groups" in {
    val r = Regex("(a)(b)?|(c)(d)?|(e)(f)?")
    "a" shouldMatchPattern { case r(Alt(0, ("a", None))) => }
    "ab" shouldMatchPattern { case r(Alt(0, ("a", Some("b")))) => }
    "c" shouldMatchPattern { case r(Alt(1, ("c", None))) => }
    "cd" shouldMatchPattern { case r(Alt(1, ("c", Some("d")))) => }
    "e" shouldMatchPattern { case r(Alt(2, ("e", None))) => }
    "ef" shouldMatchPattern { case r(Alt(2, ("e", Some("f")))) => }
  }

  it should "extract from alternatives of alternatives of different types" in {
    val r = Regex("(?:(a)|(b)(c))|(?:(d)|(e)(f))")
    "a" shouldMatchPattern { case r(Alt(0, Left("a"))) => }
    "bc" shouldMatchPattern { case r(Alt(0, Right("b", "c"))) => }
    "d" shouldMatchPattern { case r(Alt(1, Left("d"))) => }
    "ef" shouldMatchPattern { case r(Alt(1, Right("e", "f"))) => }
  }
}
