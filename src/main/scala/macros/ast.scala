package experiments.macros

import cats.syntax.all.catsSyntaxTuple2Semigroupal
import experiments.macros.hlist.{Concat, HCons, HList, HNil, ++}
import scala.quoted.{Expr, Quotes, Type}

object ast {
  sealed trait Regex[A <: HList] {
    def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[A]): (Expr[(Option[A], Boolean)], Int)
  }

  case class Capture[A <: HList: Type](inner: Regex[A]) extends Regex[HCons[String, A]] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[HCons[String, A]]): (Expr[(Option[HCons[String, A]], Boolean)], Int) = {
      val idx = Expr(i)
      val (sanitisedInner, j) = inner.sanitiseCode(groups, i + 1)
      val sanitised = '{
        val outerCap = Option($groups($idx))
        val cap = outerCap.flatMap { s =>
          val (innerCap, _) = $sanitisedInner
          innerCap.map(HCons(s, _))
        }
        (cap, outerCap.isDefined)
      }
      (sanitised, j)
    }
  }

  case class Alt[A <: HList: Type, B <: HList: Type](left: Regex[A], right: Regex[B]) extends Regex[HCons[Either[A, B], HNil]] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[HCons[Either[A, B], HNil]]): (Expr[(Option[HCons[Either[A, B], HNil.type]], Boolean)], Int) = {
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
  }

  case class Opt[A <: HList: Type](r: Regex[A]) extends Regex[HCons[Option[A], HNil]] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[HCons[Option[A], HNil]]): (Expr[(Option[HCons[Option[A], HNil.type]], Boolean)], Int) = {
      val (sanitisedValue, j) = r.sanitiseCode(groups, i)
      val sanitised = '{
        val (caps, any) = $sanitisedValue
        (Some(HCons(caps, HNil)), any)
      }
      (sanitised, j)
    }
  }

  case class Cat[A <: HList: Type, B <: HList: Type](left: Regex[A], right: Regex[B]) extends Regex[Concat[A, B]] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[Concat[A, B]]): (Expr[(Option[Concat[A, B]], Boolean)], Int) = {
      val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
      val (sanitisedRight, k) = right.sanitiseCode(groups, j)
      val sanitised = '{
        val (leftCaps, anyLeft) = $sanitisedLeft
        val (rightCaps, anyRight) = $sanitisedRight
        ((leftCaps, rightCaps).mapN(_ ++ _), anyLeft || anyRight)
      }
      (sanitised, k)
    }
  }

  case class Lit(s: String) extends Regex[HNil] {
    override def sanitiseCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes, Type[HNil]): (Expr[(Option[HNil], Boolean)], Int) = {
      val sanitised = '{ (Some(HNil), false) }
      (sanitised, i)
    }
  }
}
