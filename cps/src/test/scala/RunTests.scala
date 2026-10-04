package experiments.cps

import experiments.cps.regex.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import scala.language.implicitConversions

class RunTests extends AnyFlatSpec {
  given Conversion[Char, Lit] = Lit(_)

  behavior of "run"

  val `a` = Lit('a')
  val `(a)` = CapEmpty('a')
  val `ab` = Cat('a' +: 'b' +: RNil)
  val `(a)(b)` = Cat(CapEmpty('a') +: CapEmpty('b') +: RNil)
  val `(a(b))` = CapNonEmpty(Cat('a' +: CapEmpty('b') +: RNil))
  val `a?` = Opt('a')
  val `(a)?` = Opt(CapEmpty('a'))
  val `a|b` = Alt('a', 'b')
  val `(a)|(b)` = Alt(CapEmpty('a'), CapEmpty('b'))

  it should "match characters" in {
    `a`.run("a") shouldBe Some(())
  }

  it should "capture characters" in {
    `(a)`.run("a") shouldBe Some("a")
  }

  it should "match multiple characters" in {
    `ab`.run("ab") shouldBe Some(())
  }

  it should "capture multiple characters" in {
    `(a)(b)`.run("ab") shouldBe Some(("a", "b"))
  }

  it should "capture nested groups" in {
    `(a(b))`.run("ab") shouldBe Some(("ab", "b"))
  }

  it should "match optional patterns" in {
    `a?`.run("a") shouldBe Some(Some(()))
    `a?`.run("") shouldBe Some(None)
  }

  it should "capture optional groups" in {
    `(a)?`.run("a") shouldBe Some(Some("a"))
    `(a)?`.run("") shouldBe Some(None)
  }

  it should "match alternative patterns" in {
    `a|b`.run("a") shouldBe Some(Left(()))
    `a|b`.run("b") shouldBe Some(Right(()))
  }

  it should "capture alternative groups" in {
    `(a)|(b)`.run("a") shouldBe Some(Left("a"))
    `(a)|(b)`.run("b") shouldBe Some(Right("b"))
  }

  it should "match the whole string" in {
    `a`.run("ab") shouldBe None
  }
}
