package experiments.cps.macros

import experiments.cps.macros.regex.r
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class MacroTests extends AnyFlatSpec {
  behavior of "macro"

  val `a` = r"a"
  val `(a)` = r"(a)"
  val `ab` = r"ab"
  val `(a)(b)` = r"(a)(b)"
  val `(a(b))` = r"(a(b))"
  val `a?` = r"a?"
  val `(a)?` = r"(a)?"
  val `a|b` = r"a|b"
  val `(a)|(b)` = r"(a)|(b)"

  it should "match characters" in {
    `a`.unapply("a") shouldBe Some(())
  }

  it should "capture characters" in {
    `(a)`.unapply("a") shouldBe Some("a")
  }

  it should "match multiple characters" in {
    `ab`.unapply("ab") shouldBe Some(())
  }

  it should "capture multiple characters" in {
    `(a)(b)`.unapply("ab") shouldBe Some(("a", "b"))
  }

  it should "capture nested groups" in {
    `(a(b))`.unapply("ab") shouldBe Some(("ab", "b"))
  }

  it should "match optional patterns" in {
    `a?`.unapply("a") shouldBe Some(Some(()))
    `a?`.unapply("") shouldBe Some(None)
  }

  it should "capture optional groups" in {
    `(a)?`.unapply("a") shouldBe Some(Some("a"))
    `(a)?`.unapply("") shouldBe Some(None)
  }

  it should "match alternative patterns" in {
    `a|b`.unapply("a") shouldBe Some(Left(()))
    `a|b`.unapply("b") shouldBe Some(Right(()))
  }

  it should "capture alternative groups" in {
    `(a)|(b)`.unapply("a") shouldBe Some(Left("a"))
    `(a)|(b)`.unapply("b") shouldBe Some(Right("b"))
  }

  it should "match the whole string" in {
    `a`.unapply("ab") shouldBe None
  }
}
