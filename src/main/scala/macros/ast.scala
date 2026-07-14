package experiments.macros

import cats.syntax.all.catsSyntaxTuple2Semigroupal
import experiments.macros.hlist.{Concat, HCons, HList, HNil, Tidy, ++, tidy}
import experiments.macros.hchain.HChain
import scala.quoted.{Expr, Quotes, Type}
import parsley.templates.PureParserBridge0

object ast {

  type OptionalCapture[A <: HList] <: HList = A match {
    case HNil => HNil
    case _    => HCons[Option[Tidy[A]], HNil]
  }

  type AltCapture[A <: HList, B <: HList] <: HList = (A, B) match {
    case (HNil, HNil) => HNil
    case _            => HCons[Either[Tidy[A], Tidy[B]], HNil]
  }

  sealed trait Regex[A <: HList] {
    def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HChain[A]], Boolean)], Int)

    def getType(using Quotes): Type[A]
  }

  sealed trait Match extends Regex[HNil] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HChain[HNil]], Boolean)], Int) = empty(i)
    override def getType(using Quotes): Type[HNil] = Type.of[HNil]
  }

  type Dot = Dot.type
  case object Dot extends Match with PureParserBridge0[Dot]
  case class Lit(c: Char) extends Match

  case class Capture[A <: HList](inner: Regex[A]) extends Regex[HCons[String, A]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HChain[HCons[String, A]]], Boolean)], Int) = {
      given Type[A] = inner.getType

      val idx = Expr(i)
      val (sanitisedInner, j) = inner.sanitiseCode(groups, i + 1)
      val sanitised = '{
        val outerCap = $groups($idx)
        val cap = outerCap.flatMap { s =>
          val (innerCap, _) = $sanitisedInner
          innerCap.map(s +: _)
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
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HChain[A]], Boolean)], Int) = inner.sanitiseCode(groups, i)

    override def getType(using Quotes): Type[A] = inner.getType
  }

  case class Alt[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[AltCapture[A, B]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HChain[AltCapture[A, B]]], Boolean)], Int) = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      val (expr, j) = (Type.of[A], Type.of[B]) match {
        case ('[HNil], '[HNil]) => empty(i)
        case _                  => {
          val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
          val (sanitisedRight, k) = right.sanitiseCode(groups, j)
          val expr = '{
            val (leftCaps, anyLeft) = $sanitisedLeft
            val (rightCaps, anyRight) = $sanitisedRight
            val left = leftCaps.map(cap => Left(cap.toHList.tidy))
            val right = rightCaps.map(cap => Right(cap.toHList.tidy))
            val caps = if anyLeft then left.orElse(right) else right.orElse(left)
            (caps.map(HChain.one), anyLeft || anyRight)
          }
          (expr, k)
        }
      }
      (expr.asExprOf[(Option[HChain[AltCapture[A, B]]], Boolean)], j)
    }

    override def getType(using Quotes): Type[AltCapture[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[AltCapture[A, B]]
    }
  }

  sealed trait Optional[A <: HList](inner: Regex[A]) extends Regex[OptionalCapture[A]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HChain[OptionalCapture[A]]], Boolean)], Int) = {      
      given Type[A] = inner.getType

      val (expr, j) = Type.of[A] match {
        case '[HNil] => empty(i)
        case _       => {
          val (sanitisedInner, j) = inner.sanitiseCode(groups, i)
          val expr = '{
            val (caps, any) = $sanitisedInner
            (Some(HChain.one(caps.map(_.toHList.tidy))), any)
          }
          (expr, j)
        }
      }

      (expr.asExprOf[(Option[HChain[OptionalCapture[A]]], Boolean)], j)
    }

    override def getType(using Quotes): Type[OptionalCapture[A]] = {
      given Type[A] = inner.getType

      Type.of[OptionalCapture[A]]
    }
  }

  case class Opt[A <: HList](inner: Regex[A]) extends Optional[A](inner)
  case class Many[A <: HList](inner: Regex[A]) extends Optional[A](inner)

  case class Cat[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[Concat[A, B]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): (Expr[(Option[HChain[Concat[A, B]]], Boolean)], Int) = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      val (expr, j) = (Type.of[A], Type.of[B]) match {
        case (_, '[HNil]) => left.sanitiseCode(groups, i)
        case ('[HNil], _) => right.sanitiseCode(groups, i)
        case _            => {
          val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
          val (sanitisedRight, k) = right.sanitiseCode(groups, j)
          val expr = '{
            val (leftCaps, anyLeft) = $sanitisedLeft
            val (rightCaps, anyRight) = $sanitisedRight
            ((leftCaps, rightCaps).mapN(_ ++ _), anyLeft || anyRight)
          }
          (expr, k)
        }
      }

      // TODO: Get rid of `asExprOf`.
      (expr.asExprOf[(Option[HChain[Concat[A, B]]], Boolean)], j)
    }

    override def getType(using Quotes): Type[Concat[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[Concat[A, B]]
    }
  }

  private def empty(i: Int)(using Quotes) = {
    val expr = '{ (Some(HChain.nil), false) }
    (expr, i)
  }
}
