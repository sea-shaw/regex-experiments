package experiments.macros

import experiments.macros.extractor.extract
import experiments.macros.regex.Regex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class ExtractorUnitTests extends AnyFlatSpec {
  behavior of "Extractor"

  it should "extract from alternatives of strings" in {
    val r = Regex("(a)|(b)|(c)|(d)|(e)|(f)|(g)|(h)")
    "a" should matchPattern { case r(extract(0, "a")) => }
    "b" should matchPattern { case r(extract(1, "b")) => }
    "c" should matchPattern { case r(extract(2, "c")) => }
    "d" should matchPattern { case r(extract(3, "d")) => }
    "e" should matchPattern { case r(extract(4, "e")) => }
    "f" should matchPattern { case r(extract(5, "f")) => }
    "g" should matchPattern { case r(extract(6, "g")) => }
    "h" should matchPattern { case r(extract(7, "h")) => }
  }

  it should "extract from complex alternatives" in {
    val r = Regex("(a)(b)?|(c)(d)?|(e)(f)?")
    "a" should matchPattern { case r(extract(0, ("a", None))) => }
    "ab" should matchPattern { case r(extract(0, ("a", Some("b")))) => }
    "c" should matchPattern { case r(extract(1, ("c", None))) => }
    "cd" should matchPattern { case r(extract(1, ("c", Some("d")))) => }
    "e" should matchPattern { case r(extract(2, ("e", None))) => }
    "ef" should matchPattern { case r(extract(2, ("e", Some("f")))) => }
  }
}
