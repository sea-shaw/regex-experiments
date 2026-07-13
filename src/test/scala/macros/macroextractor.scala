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
    "a" shouldMatchPattern { case r(Alt("a")) => }
    "b" shouldMatchPattern { case r(Alt("b")) => }
    "c" shouldMatchPattern { case r(Alt("c")) => }
    "d" shouldMatchPattern { case r(Alt("d")) => }
    "e" shouldMatchPattern { case r(Alt("e")) => }
    "f" shouldMatchPattern { case r(Alt("f")) => }
    "g" shouldMatchPattern { case r(Alt("g")) => }
    "h" shouldMatchPattern { case r(Alt("h")) => }
  }

  it should "extract from alternatives of multiple capture groups" in {
    val r = Regex("(a)(b)?|(c)(d)?|(e)(f)?")
    "a" shouldMatchPattern { case r(Alt("a", None)) => }
    "ab" shouldMatchPattern { case r(Alt("a", Some("b"))) => }
    "c" shouldMatchPattern { case r(Alt("c", None)) => }
    "cd" shouldMatchPattern { case r(Alt("c", Some("d"))) => }
    "e" shouldMatchPattern { case r(Alt("e", None)) => }
    "ef" shouldMatchPattern { case r(Alt("e", Some("f"))) => }
  }

  it should "extract from alternatives of alternatives of different types" in {
    val r = Regex("(?:(a)|(b)(c))|(?:(d)|(e)(f))")
    "a" shouldMatchPattern { case r(Alt(Left("a"))) => }
    "bc" shouldMatchPattern { case r(Alt(Right("b", "c"))) => }
    "d" shouldMatchPattern { case r(Alt(Left("d"))) => }
    "ef" shouldMatchPattern { case r(Alt(Right("e", "f"))) => }
  }
}
