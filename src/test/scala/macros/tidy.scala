package experiments.macros

import experiments.macros.hcollections.hchain.{HChain, ++}
import experiments.macros.tidy.tidy
import org.scalatest.flatspec.AnyFlatSpec

import org.scalatest.matchers.should.Matchers.{compile, equal, should, shouldNot, typeCheck}

class TidyUnitTests extends AnyFlatSpec {
  behavior of "HChain tidier"

  it should "tidy empty HChain type" in {
    "val x: Unit = tidy(HChain.nil)" should compile
    "val x: Tuple1[Int] = tidy(HChain.nil)" shouldNot typeCheck

    tidy(HChain.nil) should equal (())
  }

  it should "tidy singleton HChain type" in {
    "val x: Int = tidy(HChain.one(0))" should compile
    "val x: String = tidy(HChain.one(0))" shouldNot typeCheck
    "val x: (Int, String) = tidy(HChain.one(0))" shouldNot typeCheck

    tidy(HChain.one(0)) should equal (0)
  }

  it should "tidy append HChain type" in {
    "val x: (Int, Boolean) = tidy(0 +: false +: HChain.nil)" should compile
    "val x: (Boolean, Int) = tidy(0 +: false +: HChain.nil)" shouldNot typeCheck

    tidy(0 +: false +: HChain.nil) should equal (0, false)
  }

  it should "tidy concat HChain type" in {
    "val x: (Int, Boolean, Char, String) = tidy((0 +: false +: HChain.nil) ++ ('a' +: \"b\" +: HChain.nil))" should compile
    "val x: (String, Char, Boolean, Int) = tidy((0 +: false +: HChain.nil) ++ ('a' +: \"b\" +: HChain.nil))" shouldNot typeCheck

    tidy((0 +: false +: HChain.nil) ++ ('a' +: "b" +: HChain.nil)) should equal (0, false, 'a', "b")
  }

  it should "tidy HChain with nested Option of singleton" in {
    "val x: Option[String] = tidy(HChain.one(Some(HChain.one(\"a\"))))" should compile
    "val x: Option[Int] = tidy(HChain.one(Some(HChain.one(\"a\"))))" shouldNot typeCheck
    "val x: Option[HSingleton[String]] = tidy(HChain.one(Some(HChain.one(\"a\"))))" shouldNot typeCheck

    tidy(HChain.one(Some(HChain.one("a")))) should equal (Some("a"))
  }
}
