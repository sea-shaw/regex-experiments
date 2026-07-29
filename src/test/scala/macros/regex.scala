package experiments.macros

import cats.data.Ior.{Both => IBoth, Left => ILeft, Right => IRight}
import experiments.macros.regex.r
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class RegexMacroUnitTests extends AnyFlatSpec {

  behavior of "Regex macro"

  it should "match zero capture groups" in {
    val r = r"a"
    "a" should matchPattern { case r(()) => } // TODO: Make this look nicer
  }

  it should "match one capture group" in {
    val r = r"(a)"
    "a" should matchPattern { case r("a") => }
  }

  it should "match multiple capture groups" in {
    val r = r"(a)(b)(c)"
    "abc" should matchPattern { case r("a", "b", "c") => }
  }

  it should "match nested capture groups" in {
    val r = r"(a(b(c)d)e)"
    "abcde" should matchPattern { case r("abcde", "bcd", "c") => }
  }

  it should "match optional patterns" in {
    val r = r"a?" // TODO: Make type `Unit` instead of `Option[Unit]`
    "a" should matchPattern { case r(()) => }
    "" should matchPattern { case r(()) => }
  }

  it should "match optional capture groups" in {
    val r = r"(a)?"
    "a" should matchPattern { case r(Some("a")) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match nested optional capture groups" in {
    val r = r"(a(b)?)?"
    "" should matchPattern { case r(None) => }
    "a" should matchPattern { case r(Some("a", None)) => }
    "ab" should matchPattern { case r(Some("ab", Some("b"))) => }
  }

  it should "match star capture groups" in {
    val r = r"(a)*"
    "aaaa" should matchPattern { case r(Some("a")) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match alternative patterns" in {
    val r = r"a|b" // TODO: Make type `Unit` instead of `Either[Unit, Unit]`
     "a" should matchPattern { case r(()) => }
     "b" should matchPattern { case r(()) => }
  }

  it should "match alternative capture groups" in {
    val r = r"(a)|(b)"
    "a" should matchPattern { case r(Left("a")) => }
    "b" should matchPattern { case r(Right("b")) => }
  }

  it should "match alternative patterns with capture groups on one side" in {
    val r = r"(a)|b"
    "a" should matchPattern { case r(Left("a")) => }
    "b" should matchPattern { case r(Right(())) => }
  }

  it should "match alternatives with multiple capture groups on either side" in {
    val r = r"(a)(b)|(c)(d)"
    "ab" should matchPattern { case r(Left("a", "b")) => }
    "cd" should matchPattern { case r(Right("c", "d")) => }
  }

  it should "match many chained alternative capture groups" in {
    val r = r"(a)|(b)|(c)|(d)"
    "a" should matchPattern { case r(Left("a")) => }
    "b" should matchPattern { case r(Right(Left("b"))) => }
    "c" should matchPattern { case r(Right(Right(Left("c")))) => }
    "d" should matchPattern { case r(Right(Right(Right("d")))) => }
  }

  it should "allow non-capturing groups" in {
    val r = r"(?:a)"
    "a" should matchPattern { case r(()) => }
  }

  it should "match capture groups with shared optionality" in {
    val r = r"(?:(a)(b))?"
    "ab" should matchPattern { case r(Some("a", "b")) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match optional capture groups inside alternative" in {
    val r = r"(a)?|(b)?"
    "a" should matchPattern { case r(Left(Some("a"))) => }
    "b" should matchPattern { case r(Right(Some("b"))) => }
    "" should matchPattern { case r(Left(None)) | r(Right(None)) => }
  }

  it should "match alternative capture groups inside optional" in {
    val r = r"(?:(a)|(b))?"
    "a" should matchPattern { case r(Some(Left("a"))) => }
    "b" should matchPattern { case r(Some(Right("b"))) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match nested alternative capture groups" in {
    val r = r"(?:(a)|(b))|(?:(c)|(d))"
    "a" should matchPattern { case r(Left(Left("a"))) => }
    "b" should matchPattern { case r(Left(Right("b"))) => }
    "c" should matchPattern { case r(Right(Left("c"))) => }
    "d" should matchPattern { case r(Right(Right("d"))) => }
  }

  it should "match the left capture group in one or more alternatives" in {
    val r = r"((a)|(b))+"
    "a" should matchPattern { case r("a", ILeft("a")) => }
  }

  it should "match the right capture group in one or more alternatives" in {
    val r = r"((a)|(b))+"
    "b" should matchPattern { case r("b", IRight("b")) => }
  }

  it should "match both capture groups in one or more alternatives" in {
    val r = r"((a)|(b))+"
    "ba" should matchPattern { case r("a", IBoth("a", "b")) => }
    "ab" should matchPattern { case r("b", IBoth("a", "b")) => }
  }

  it should "match the left capture group in zero or more alternatives" in {
    val r = r"((a)|(b))*"
    "a" should matchPattern { case r(Some("a", ILeft("a"))) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match the right capture group in zero or more alternatives" in {
    val r = r"((a)|(b))*"
    "b" should matchPattern { case r(Some("b", IRight("b"))) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match both capture groups in zero or more alternatives" in {
    val r = r"((a)|(b))*"
    "ba" should matchPattern { case r(Some("a", IBoth("a", "b"))) => }
    "ab" should matchPattern { case r(Some("b", IBoth("a", "b"))) => }
    "" should matchPattern { case r(None) => }
  }
}
