package experiments.macros

import experiments.macros.regex.Regex
import experiments.macros.hlist.{HCons, HNil}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class RegexMacroUnitTests extends AnyFlatSpec {

  behavior of "Regex macro"

  it should "match zero capture groups" in {
    val r = Regex("a")
    "a" should matchPattern { case r(HNil) => } // TODO: Make this look nicer
  }

  it should "match one capture group" in {
    val r = Regex("(a)")
    "a" should matchPattern { case r(HCons("a", HNil)) => }
  }

  it should "match multiple capture groups" in {
    val r = Regex("(a)(b)(c)")
    "abc" should matchPattern { case r(HCons("a", HCons("b", HCons("c", HNil)))) => }
  }

  it should "match nested capture groups" in {
    val r = Regex("(a(b(c)d)e)")
    "abcde" should matchPattern { case r(HCons("abcde", HCons("bcd", HCons("c", HNil)))) => }
  }

  it should "match optional capture groups" in {
    val r = Regex("(a)?")
    "a" should matchPattern { case r(HCons(Some(HCons("a", HNil)), HNil)) => }
    "" should matchPattern { case r(HCons(None, HNil)) => }
  }

  it should "match nested optional capture groups" in {
    val r = Regex("(a(b)?)?")
    "" should matchPattern { case r(HCons(None, HNil)) => }
    "a" should matchPattern { case r(HCons(Some(HCons("a", HCons(None, HNil))), HNil)) => }
    "ab" should matchPattern { case r(HCons(Some(HCons("ab", HCons(Some(HCons("b", HNil)), HNil))), HNil)) => }
  }

  it should "match star capture groups" in {
    val r = Regex("(a)*")
    "aaaa" should matchPattern { case r(HCons(Some(HCons("a", HNil)), HNil)) => }
    "" should matchPattern { case r(HCons(None, HNil)) => }
  }

  it should "match alternative capture groups" in {
    val r = Regex("(a)|(b)")
    "a" should matchPattern { case r(HCons(Left(HCons("a", HNil)), HNil)) => }
    "b" should matchPattern { case r(HCons(Right(HCons("b", HNil)), HNil)) => }
  }

  it should "match alternatives with multiple capture groups on either side" in {
    val r = Regex("(a)(b)|(c)(d)")
    "ab" should matchPattern { case r(HCons(Left(HCons("a", HCons("b", HNil))), HNil)) => }
    "cd" should matchPattern { case r(HCons(Right(HCons("c", HCons("d", HNil))), HNil)) => }
  }

  it should "match many chained alternative capture groups" in {
    val r = Regex("(a)|(b)|(c)|(d)")
    "a" should matchPattern { case r(HCons(Left(HCons("a", HNil)), HNil)) => }
    "b" should matchPattern { case r(HCons(Right(HCons(Left(HCons("b", HNil)), HNil)), HNil)) => }
    "c" should matchPattern { case r(HCons(Right(HCons(Right(HCons(Left(HCons("c", HNil)), HNil)), HNil)), HNil)) => }
    "d" should matchPattern { case r(HCons(Right(HCons(Right(HCons(Right(HCons("d", HNil)), HNil)), HNil)), HNil)) => }
  }

  it should "allow non-capturing groups" in {
    val r = Regex("(?:a)")
    "a" should matchPattern { case r(HNil) => }
  }

  it should "match capture groups with shared optionality" in {
    val r = Regex("(?:(a)(b))?")
    "ab" should matchPattern { case r(HCons(Some(HCons("a", HCons("b", HNil))), HNil)) => }
    "" should matchPattern { case r(HCons(None, HNil)) => }
  }

  it should "match optional capture groups inside alternative" in {
    val r = Regex("(a)?|(b)?")
    "a" should matchPattern { case r(HCons(Left(HCons(Some(HCons("a", HNil)), HNil)), HNil)) => }
    "b" should matchPattern { case r(HCons(Right(HCons(Some(HCons("b", HNil)), HNil)), HNil)) => }
    "" should matchPattern { case r(HCons(Left(HCons(None, HNil)), HNil)) | r(HCons(Right(HCons(None, HNil)), HNil)) => }
  }

  it should "match alternative capture groups inside optional" in {
    val r = Regex("(?:(a)|(b))?")
    "a" should matchPattern { case r(HCons(Some(HCons(Left(HCons("a", HNil)), HNil)), HNil)) => }
    "b" should matchPattern { case r(HCons(Some(HCons(Right(HCons("b", HNil)), HNil)), HNil)) => }
    "" should matchPattern { case r(HCons(None, HNil)) => }
  }

  it should "match nested alternative capture groups" in {
    val r = Regex("(?:(a)|(b))|(?:(c)|(d))")
    "a" should matchPattern { case r(HCons(Left(HCons(Left(HCons("a", HNil)), HNil)), HNil)) => }
    "b" should matchPattern { case r(HCons(Left(HCons(Right(HCons("b", HNil)), HNil)), HNil)) => }
    "c" should matchPattern { case r(HCons(Right(HCons(Left(HCons("c", HNil)), HNil)), HNil)) => }
    "d" should matchPattern { case r(HCons(Right(HCons(Right(HCons("d", HNil)), HNil)), HNil)) => }
  }
}
