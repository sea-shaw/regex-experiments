package experiments

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import experiments.matchtypes.Regex

class MatchTypesUnitTests extends AnyFlatSpec {

  behavior of "Regex"

  it should "match zero capture groups" in {
    val r = Regex("a")
    "a" should matchPattern { case r(()) => } // TODO: Make this look nicer
  }

  it should "match one capture group" in {
    val r = Regex("(a)")
    "a" should matchPattern { case r("a") => }
  }

  it should "match multiple capture groups" in {
    val r = Regex("(a)(b)(c)")
    "abc" should matchPattern { case r("a", "b", "c") => }
  }

  it should "match nested capture groups" in {
    val r = Regex("(a(b(c)d)e)")
    "abcde" should matchPattern { case r("abcde", "bcd", "c") => }
  }

  it should "match optional capture groups" in {
    val r = Regex("(a)?")
    "a" should matchPattern { case r(Some("a")) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match star capture groups" in {
    val r = Regex("(a)*")
    "aaaa" should matchPattern { case r(Some("a")) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match alternative capture groups" in {
    val aOrB = Regex("(a)|(b)")
    "a" should matchPattern { case aOrB(Left("a")) => }
    "b" should matchPattern { case aOrB(Right("b")) => }
  }

  it should "match many alternative capture groups" in {
    val r = Regex("(a)|(b)|(c)|(d)")
    "a" should matchPattern { case r(Left("a")) => }
    "b" should matchPattern { case r(Right(Left("b"))) => }
    "c" should matchPattern { case r(Right(Right(Left("c")))) => }
    "d" should matchPattern { case r(Right(Right(Right("d")))) => }
  }

  it should "allow non-capturing groups" in {
    val r = Regex("(?:a)")
    "a" should matchPattern { case r(()) => }
  }

  it should "match capture groups with shared optionality" in {
    val r = Regex("(?:(a)(b))?")
    "ab" should matchPattern { case r(Some(("a", "b"))) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match optional capture groups inside alternative" in {
    val r = Regex("(a)?|(b)?")
    "a" should matchPattern { case r(Left(Some("a"))) => }
    "b" should matchPattern { case r(Right(Some("b"))) => }
    "" should matchPattern { case r(Left(None)) | r(Right(None)) => }
  }

  it should "match alternative capture groups inside optional" in {
    val r = Regex("(?:(a)|(b))?")
    "a" should matchPattern { case r(Some(Left("a"))) => }
    "b" should matchPattern { case r(Some(Right("b"))) => }
    "" should matchPattern { case r(None) => }
  }
}
