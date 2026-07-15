package experiments.macros

import cats.collections.Diet
import cats.syntax.all.catsSyntaxTuple2Semigroupal
import experiments.macros.hlist.{Concat, HCons, HList, HNil, Tidy, tidy}
import experiments.macros.hchain.HChain
import experiments.macros.matching.resolveMatchType
import scala.quoted.{Expr, Quotes, Type}
import parsley.templates.PureParserBridge0

object ast {

  type OptionalCapture[A <: HList] <: HList = A match {
    case HNil => HNil
    case _    => HCons[Option[Tidy[A]], HNil]
  }

  // Need to use tuple of types since match type macro can only handle 1 tpe parameter.
  type AltCapture[T <: (?, ?)] = T match {
    case (HNil, HNil) => HNil
    case _            => HCons[Either[Tidy[Fst[T]], Tidy[Snd[T]]], HNil]
  }

  type Fst[T <: (?, ?)] = Tuple.Elem[T, 0]
  type Snd[T <: (?, ?)] = Tuple.Elem[T, 1]

  type SanitiseCode[A <: HList] = (Expr[(Option[HChain[A]], Boolean)], Int)

  sealed trait Regex[A <: HList] {
    def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): SanitiseCode[A]

    def getType(using Quotes): Type[A]
  }

  sealed trait Match extends Regex[HNil] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): SanitiseCode[HNil] = empty(i)
    override def getType(using Quotes): Type[HNil] = Type.of[HNil]
  }

  type Dot = Dot.type
  case object Dot extends Match with PureParserBridge0[Dot]
  case class Lit(c: Int) extends Match
  case class Class(cs: Diet[Int]) extends Match

  case class Capture[A <: HList](inner: Regex[A]) extends Regex[HCons[String, A]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): SanitiseCode[HCons[String, A]] = {
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

  sealed trait CaptureInner[A <: HList](inner: Regex[A]) extends Regex[A] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): SanitiseCode[A] = inner.sanitiseCode(groups, i)

    override def getType(using Quotes): Type[A] = inner.getType
  }

  case class NonCapture[A <: HList](inner: Regex[A]) extends CaptureInner[A](inner)
  case class Rep1[A <: HList](inner: Regex[A]) extends CaptureInner[A](inner)

  case class Alt[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[AltCapture[(A, B)]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): SanitiseCode[AltCapture[(A, B)]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      resolveMatchType[(A, B), AltCapture, SanitiseCode] {
        Type.of[(A, B)] match {
          case '[(HNil, HNil)] => empty(i)
          case '[Any] => {
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
      }
    }

    override def getType(using Quotes): Type[AltCapture[(A, B)]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[AltCapture[(A, B)]]
    }
  }

  sealed trait Optional[A <: HList](inner: Regex[A]) extends Regex[OptionalCapture[A]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): SanitiseCode[OptionalCapture[A]] = {      
      given Type[A] = inner.getType

      resolveMatchType[A, OptionalCapture, SanitiseCode] {
        Type.of[A] match {
          case '[HNil] => empty(i)
          case '[Any]  => {
            val (sanitisedInner, j) = inner.sanitiseCode(groups, i)
            val expr = '{
              val (caps, any) = $sanitisedInner
              (Some(HChain.one(caps.map(_.toHList.tidy))), any)
            }
            (expr, j)
          }
        }
      }
    }

    override def getType(using Quotes): Type[OptionalCapture[A]] = {
      given Type[A] = inner.getType

      Type.of[OptionalCapture[A]]
    }
  }

  case class Opt[A <: HList](inner: Regex[A]) extends Optional[A](inner)
  case class Rep0[A <: HList](inner: Regex[A]) extends Optional[A](inner)

  case class Cat[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[Concat[A, B]] {
    override def sanitiseCode(groups: Expr[Array[Option[String]]], i: Int)(using Quotes): SanitiseCode[Concat[A, B]] = {
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

  private def empty(i: Int)(using Quotes): SanitiseCode[HNil] = {
    val expr = '{ (Some(HChain.nil), false) }
    (expr, i)
  }
}
