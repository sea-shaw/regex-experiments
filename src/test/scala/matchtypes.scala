package experiments

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import experiments.matchtypes.Regex

class MatchTypesUnitTests extends AnyFlatSpec {

  behavior of "Regex"

  it should "match simple capture groups" in {
    val a = Regex("(a)")
    "a" should matchPattern {
      case a("a") =>
    }
    val ab = Regex("(a)(b)")
    "ab" should matchPattern {
      case ab("a", "b") =>
    }
  }

  it should "match nested capture groups" in {
    val ab = Regex("(a(b))")
    "ab" should matchPattern {
      case ab("ab", "b") =>
    }
    val abcde = Regex("(a(b(c)d)e)")
    "abcde" should matchPattern {
      case abcde("abcde", "bcd", "c") =>
    }
  }

  it should "match optional capture groups" in {
    val optA = Regex("(a)?")
    "a" should matchPattern {
      case optA(Some("a")) =>
    }
    "" should matchPattern {
      case optA(None) =>
    }
  }

  it should "match zero or more capture groups" in {
    val aStar = Regex("(a)*")
    "aaaa" should matchPattern {
      case aStar(Some("a")) =>
    }
    "" should matchPattern {
      case aStar(None) =>
    }
  }

  it should "match alternative capture groups" in {
    val aOrB = Regex("(a)|(b)")
    "a" should matchPattern {
      case aOrB(Left("a")) =>
    }
    "b" should matchPattern {
      case aOrB(Right("b")) =>
    }
  }
}
