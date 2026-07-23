package experiments.macros

import cats.data.Ior
import cats.data.Ior.{Both => IBoth, Left => ILeft, Right => IRight}
import experiments.macros.hcollections.hchain.{HChain, HSingleton, HAppend, ++}
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

  it should "tidy HChain with nested Option" in {
    "val x: Option[String] = tidy(HChain.one(Some(HChain.one(\"a\"))))" should compile
    "val x: Option[Int] = tidy(HChain.one(Some(HChain.one(\"a\"))))" shouldNot typeCheck
    "val x: Option[HSingleton[String]] = tidy(HChain.one(Some(HChain.one(\"a\"))))" shouldNot typeCheck

    tidy(HChain.one(Some(HChain.one("a")))) should equal (Some("a"))
  }

  it should "tidy HChain with nested Either" in {
    """
    val xs: HSingleton[Either[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(Left(HChain.one("a")))
    val x: Either[String, (String, String)] = tidy(xs)
    """ should compile

    """
    val ys: HSingleton[Either[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(Right(HChain.one("b") ++ HChain.one("c")))
    val y: Either[String, (String, String)] = tidy(ys)
    """ should compile

    """
    val xs: HSingleton[Either[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(Left(HChain.one("a")))
    val x: Either[(String, String), String] = tidy(xs)
    """ shouldNot typeCheck

    """
    val ys: HSingleton[Either[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(Right(HChain.one("b") ++ HChain.one("c")))
    val y: Either[(String, String), String] = tidy(ys)
    """ shouldNot typeCheck

    val xs: HSingleton[Either[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(Left(HChain.one("a")))
    tidy(xs) should equal (Left("a"))

    val ys: HSingleton[Either[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(Right(HChain.one("b") ++ HChain.one("c")))
    tidy(ys) should equal (Right("b", "c"))
  }

  it should "tidy HChain with nested Ior" in {
    """
    val xs: HSingleton[Ior[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(ILeft(HChain.one("a")))
    val x: Ior[String, (String, String)] = tidy(xs)
    """ should compile

    """
    val ys: HSingleton[Ior[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(IRight(HChain.one("b") ++ HChain.one("c")))
    val y: Ior[String, (String, String)] = tidy(ys)
    """ should compile

    """
    val zs: HSingleton[Ior[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(IBoth(HChain.one("a"), HChain.one("b") ++ HChain.one("c")))
    val z: Ior[String, (String, String)] = tidy(zs)
    """ should compile

    """
    val xs: HSingleton[Either[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(ILeft(HChain.one("a")))
    val x: Ior[(String, String), String] = tidy(xs)
    """ shouldNot typeCheck

    """
    val ys: HSingleton[Ior[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(IRight(HChain.one("b") ++ HChain.one("c")))
    val y: Ior[(String, String), String] = tidy(ys)
    """ shouldNot typeCheck

    """
    val zs: HSingleton[Ior[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(IBoth(HChain.one("a"), HChain.one("b") ++ HChain.one("c")))
    val z: Ior[(String, String), String] = tidy(zs)
    """ shouldNot typeCheck

    val xs: HSingleton[Ior[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(ILeft(HChain.one("a")))
    tidy(xs) should equal (ILeft("a"))

    val ys: HSingleton[Ior[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(IRight(HChain.one("b") ++ HChain.one("c")))
    tidy(ys) should equal (IRight("b", "c"))

    val zs: HSingleton[Ior[HSingleton[String], HAppend[HSingleton[String], HSingleton[String]]]] = HChain.one(IBoth(HChain.one("a"), HChain.one("b") ++ HChain.one("c")))
    tidy(zs) should equal (IBoth("a", ("b", "c")))
  }
}
