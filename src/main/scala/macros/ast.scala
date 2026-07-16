package experiments.macros

import cats.collections.Diet
import cats.kernel.Order
import cats.syntax.all.*
import experiments.macros.hlist.{Concat, HCons, HList, HNil, Tidy}
import experiments.macros.hchain.HChain
import parsley.templates.PureParserBridge0
import scala.quoted.{Expr, Quotes, Type}

object ast {

  type Groups = Array[Option[(String, Int)]]

  type OptionalCapture[A <: HList] <: HList = A match {
    case HNil => HNil
    case _    => SingleOption[A]
  }

  type SingleOption[A <: HList] = HCons[Option[Tidy[A]], HNil]

  type AltCapture[A <: HList, B <: HList] <: HList = (A, B) match {
    case (HNil, HNil) => HNil
    case _            => SingleEither[A, B]
  }

  type SingleEither[A <: HList, B <: HList] = HCons[Either[Tidy[A], Tidy[B]], HNil]

  case class Sanitised[A <: HList](captures: HChain[A], any: Option[Int])
  object Sanitised {
    given [A <: HList] => Order[Sanitised[A]] = Order.by(_.any)
  }

  type SanitiseExpr[A <: HList] = Expr[Option[Sanitised[A]]]
  type SanitiseCode[A <: HList] = (SanitiseExpr[A], Int)

  sealed trait Regex[A <: HList] {
    def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[A]

    def getType(using Quotes): Type[A]
  }

  sealed trait Match extends Regex[HNil] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HNil] = empty(i)
    override def getType(using Quotes): Type[HNil] = Type.of[HNil]
  }

  type Dot = Dot.type
  case object Dot extends Match with PureParserBridge0[Dot]
  case class Lit(c: Int) extends Match
  case class Class(cs: Diet[Int]) extends Match

  case class Capture[A <: HList](inner: Regex[A]) extends Regex[HCons[String, A]] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HCons[String, A]] = {
      given Type[A] = inner.getType

      val idx = Expr(i)
      val (sanitisedInner, j) = inner.sanitiseCode(groups, i + 1)
      val sanitised = '{
        $groups($idx).flatMap { (s, end) =>
          val innerCap = $sanitisedInner
          innerCap.map { case Sanitised(caps, _) =>
            Sanitised(s +: caps, Some(end))
          }
        }
      }
      (sanitised, j)
    }

    override def getType(using Quotes): Type[HCons[String, A]] = {
      given Type[A] = inner.getType
      Type.of[HCons[String, A]]
    }
  }

  sealed trait CaptureInner[A <: HList](inner: Regex[A]) extends Regex[A] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[A] = inner.sanitiseCode(groups, i)

    override def getType(using Quotes): Type[A] = inner.getType
  }

  case class NonCapture[A <: HList](inner: Regex[A]) extends CaptureInner[A](inner)
  case class Rep1[A <: HList](inner: Regex[A]) extends CaptureInner[A](inner)

  case class Alt[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[AltCapture[A, B]] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[AltCapture[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      val (expr, j) = (Type.of[A], Type.of[B]) match {
        case ('[HNil], '[HNil]) => empty(i)
        case _                  => {
          val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
          val (sanitisedRight, k) = right.sanitiseCode(groups, j)
          val expr: SanitiseExpr[SingleEither[A, B]] = '{
            val left = $sanitisedLeft.map { case Sanitised(leftCaps, anyLeft) =>
              Sanitised(HChain.one(leftCaps.tidy.asLeft[Tidy[B]]), anyLeft)
            }
            val right = $sanitisedRight.map { case Sanitised(rightCaps, anyRight) =>
              Sanitised(HChain.one(rightCaps.tidy.asRight[Tidy[A]]), anyRight)
            }
            left max right
          }
          (expr, k)
        }
      }
      (expr.asExprOf[Option[Sanitised[AltCapture[A, B]]]], j)
    }

    override def getType(using Quotes): Type[AltCapture[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[AltCapture[A, B]]
    }
  }

  sealed trait Optional[A <: HList](inner: Regex[A]) extends Regex[OptionalCapture[A]] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[OptionalCapture[A]] = {      
      given Type[A] = inner.getType

      val (expr, j) = Type.of[A] match {
        case '[HNil] => empty(i)
        case _       => {
          val (sanitisedInner, j) = inner.sanitiseCode(groups, i)
          val expr: SanitiseExpr[SingleOption[A]] = '{
            val innerCaps = $sanitisedInner
            val innerAny = innerCaps.fold(None)(_.any)
            Some(Sanitised(HChain.one(innerCaps.map(_.captures.tidy)), innerAny))
          }
          (expr, j)
        }
      }

      (expr.asExprOf[Option[Sanitised[OptionalCapture[A]]]], j)
    }

    override def getType(using Quotes): Type[OptionalCapture[A]] = {
      given Type[A] = inner.getType

      Type.of[OptionalCapture[A]]
    }
  }

  case class Opt[A <: HList](inner: Regex[A]) extends Optional[A](inner)
  case class Rep0[A <: HList](inner: Regex[A]) extends Optional[A](inner)

  case class Cat[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[Concat[A, B]] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[Concat[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      val (expr, j) = (Type.of[A], Type.of[B]) match {
        case (_, '[HNil]) => left.sanitiseCode(groups, i)
        case ('[HNil], _) => right.sanitiseCode(groups, i)
        case _            => {
          val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
          val (sanitisedRight, k) = right.sanitiseCode(groups, j)
          val expr: SanitiseExpr[Concat[A, B]] = '{
            for {
              Sanitised(leftCaps, anyLeft) <- $sanitisedLeft
              Sanitised(rightCaps, anyRight) <- $sanitisedRight
            } yield Sanitised(leftCaps ++ rightCaps, anyLeft max anyRight)
          }
          (expr, k)
        }
      }

      (expr.asExprOf[Option[Sanitised[Concat[A, B]]]], j)
    }

    override def getType(using Quotes): Type[Concat[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[Concat[A, B]]
    }
  }

  private def empty(i: Int)(using Quotes): SanitiseCode[HNil] = {
    val expr = '{ (Some(Sanitised(HChain.nil, None))) }
    (expr, i)
  }
}
