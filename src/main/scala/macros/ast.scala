package experiments.macros

import cats.collections.Diet
import cats.syntax.all.catsSyntaxTuple2Semigroupal
import experiments.macros.hlist.{Concat, HCons, HList, HNil, ++}
import scala.quoted.{Expr, Quotes, Type}

object ast {
  sealed trait Regex[A <: HList] {
    def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[A], Boolean)], Int)

    def getType(using Quotes): Type[A]
  }

  sealed trait Match extends Regex[HNil] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HNil], Boolean)], Int) = {
      val sanitised = '{ (Some(HNil), false) }
      (sanitised, i)
    }
    override def getType(using Quotes): Type[HNil] = Type.of[HNil]
  }

  case object Dot extends Match
  case class Lit(c: Int) extends Match
  case class Range(cs: Diet[Int]) extends Match

  case class Capture[A <: HList](inner: Regex[A]) extends Regex[HCons[String, A]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HCons[String, A]], Boolean)], Int) = {
      given Type[A] = inner.getType

      val idx = Expr(i)
      val (sanitisedInner, j) = inner.sanitiseCode(groups, i + 1)
      val sanitised = '{
        val outerCap = $groups($idx)
        val cap = outerCap.flatMap { s =>
          val (innerCap, _) = $sanitisedInner
          innerCap.map(HCons(s, _))
        }
        (cap, outerCap.isDefined)
      }
      (sanitised, j)
    }

    override def getType(using Quotes): Type[HCons[String, A]] = {
      given Type[A] = inner.getType
      Type.of[HCons[String, A]]
    }
  }

  case class NonCapture[A <: HList](inner: Regex[A]) extends Regex[A] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[A], Boolean)], Int) = inner.sanitiseCode(groups, i)

    override def getType(using Quotes): Type[A] = inner.getType
  }

  case class Alt[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[HCons[Either[A, B], HNil]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HCons[Either[A, B], HNil]], Boolean)], Int) = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
      val (sanitisedRight, k) = right.sanitiseCode(groups, j)
      val sanitised = '{
        val (leftCaps, anyLeft) = $sanitisedLeft
        val (rightCaps, anyRight) = $sanitisedRight
        val left = leftCaps.map(Left(_))
        val right = rightCaps.map(Right(_))
        val caps = if anyLeft then left.orElse(right) else right.orElse(left)
        (caps.map(HCons(_, HNil)), anyLeft || anyRight)
      }
      (sanitised, k)
    }

    override def getType(using Quotes): Type[HCons[Either[A, B], HNil]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[HCons[Either[A, B], HNil]]
    }
  }

  case class Opt[A <: HList](inner: Regex[A]) extends Regex[HCons[Option[A], HNil]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HCons[Option[A], HNil]], Boolean)], Int) = {
      given Type[A] = inner.getType

      val (sanitisedValue, j) = inner.sanitiseCode(groups, i)
      val sanitised = '{
        val (caps, any) = $sanitisedValue
        (Some(HCons(caps, HNil)), any)
      }
      (sanitised, j)
    }

    override def getType(using Quotes): Type[HCons[Option[A], HNil]] = {
      given Type[A] = inner.getType

      Type.of[HCons[Option[A], HNil]]
    }
  }

  case class Cat[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[Concat[A, B]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[Concat[A, B]], Boolean)], Int) = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
      val (sanitisedRight, k) = right.sanitiseCode(groups, j)
      val sanitised = '{
        val (leftCaps, anyLeft) = $sanitisedLeft
        val (rightCaps, anyRight) = $sanitisedRight
        ((leftCaps, rightCaps).mapN(_ ++ _), anyLeft || anyRight)
      }
      (sanitised, k)
    }

    override def getType(using Quotes): Type[Concat[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[Concat[A, B]]
    }
  }
}
