package experiments.macros

import cats.collections.Diet
import experiments.macros.hlist.{Concat, HCons, HList, HNil, Tidy, tidy}
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

  // TODO: Remove posibility of `(None, Some[Int])`
  type SanitiseCode[A <: HList] = (Expr[Option[(HChain[A], Option[Int])]], Int)

  sealed trait Regex[A <: HList] {
    def sanitiseCode(groups: Expr[Array[Option[(String, Int)]]], i: Int)(using Quotes): SanitiseCode[A]

    def getType(using Quotes): Type[A]
  }

  sealed trait Match extends Regex[HNil] {
    override def sanitiseCode(groups: Expr[Array[Option[(String, Int)]]], i: Int)(using Quotes): SanitiseCode[HNil] = empty(i)
    override def getType(using Quotes): Type[HNil] = Type.of[HNil]
  }

  type Dot = Dot.type
  case object Dot extends Match with PureParserBridge0[Dot]
  case class Lit(c: Int) extends Match
  case class Class(cs: Diet[Int]) extends Match

  case class Capture[A <: HList](inner: Regex[A]) extends Regex[HCons[String, A]] {
    override def sanitiseCode(groups: Expr[Array[Option[(String, Int)]]], i: Int)(using Quotes): SanitiseCode[HCons[String, A]] = {
      given Type[A] = inner.getType

      val idx = Expr(i)
      val (sanitisedInner, j) = inner.sanitiseCode(groups, i + 1)
      val sanitised = '{
        $groups($idx).flatMap { (s, end) =>
          val innerCap = $sanitisedInner
          innerCap.map { case (caps, _) =>
            (s +: caps, Some(end))
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
    override def sanitiseCode(groups: Expr[Array[Option[(String, Int)]]], i: Int)(using Quotes): SanitiseCode[A] = inner.sanitiseCode(groups, i)

    override def getType(using Quotes): Type[A] = inner.getType
  }

  case class NonCapture[A <: HList](inner: Regex[A]) extends CaptureInner[A](inner)
  case class Rep1[A <: HList](inner: Regex[A]) extends CaptureInner[A](inner)

  case class Alt[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[AltCapture[A, B]] {
    override def sanitiseCode(groups: Expr[Array[Option[(String, Int)]]], i: Int)(using Quotes): SanitiseCode[AltCapture[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      val (expr, j) = (Type.of[A], Type.of[B]) match {
        case ('[HNil], '[HNil]) => empty(i)
        case _                  => {
          val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
          val (sanitisedRight, k) = right.sanitiseCode(groups, j)
          val expr = '{
            ($sanitisedLeft, $sanitisedRight) match {
              case (Some(leftCaps, anyLeft), Some(rightCaps, anyRight)) => {
                val left = Left(leftCaps.toHList.tidy)
                val right = Right(rightCaps.toHList.tidy)
                val caps = if Ordering[Option[Int]].gt(anyLeft, anyRight) then left else right
                Some(HChain.one(caps), Ordering[Option[Int]].max(anyLeft, anyRight))
              }
              case (Some(leftCaps, anyLeft), None)                      => {
                Some(HChain.one(Left(leftCaps.toHList.tidy)), anyLeft)
              }
              case (None, Some(rightCaps, anyRight))                    => {
                Some(HChain.one(Right(rightCaps.toHList.tidy)), anyRight)
              }
              case (None, None)                                         => {
                None
              }
            }
          }
          (expr, k)
        }
      }
      (expr.asExprOf[Option[(HChain[AltCapture[A, B]], Option[Int])]], j)
    }

    override def getType(using Quotes): Type[AltCapture[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[AltCapture[A, B]]
    }
  }

  sealed trait Optional[A <: HList](inner: Regex[A]) extends Regex[OptionalCapture[A]] {
    override def sanitiseCode(groups: Expr[Array[Option[(String, Int)]]], i: Int)(using Quotes): SanitiseCode[OptionalCapture[A]] = {      
      given Type[A] = inner.getType

      val (expr, j) = Type.of[A] match {
        case '[HNil] => empty(i)
        case _       => {
          val (sanitisedInner, j) = inner.sanitiseCode(groups, i)
          val expr: Expr[Some[(HChain[HCons[Option[Tidy[A]], HNil]], Option[Int])]] = '{
            val innerCaps = $sanitisedInner
            val innerAny = innerCaps.map(_._2).getOrElse(None)
            Some(HChain.one(innerCaps.map(_._1.toHList.tidy)), innerAny)
          }
          (expr, j)
        }
      }

      (expr.asExprOf[Option[(HChain[OptionalCapture[A]], Option[Int])]], j)
    }

    override def getType(using Quotes): Type[OptionalCapture[A]] = {
      given Type[A] = inner.getType

      Type.of[OptionalCapture[A]]
    }
  }

  case class Opt[A <: HList](inner: Regex[A]) extends Optional[A](inner)
  case class Rep0[A <: HList](inner: Regex[A]) extends Optional[A](inner)

  case class Cat[A <: HList, B <: HList](left: Regex[A], right: Regex[B]) extends Regex[Concat[A, B]] {
    override def sanitiseCode(groups: Expr[Array[Option[(String, Int)]]], i: Int)(using Quotes): SanitiseCode[Concat[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      val (expr, j) = (Type.of[A], Type.of[B]) match {
        case (_, '[HNil]) => left.sanitiseCode(groups, i)
        case ('[HNil], _) => right.sanitiseCode(groups, i)
        case _            => {
          val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
          val (sanitisedRight, k) = right.sanitiseCode(groups, j)
          val expr = '{
            for {
              (leftCaps, anyLeft) <- $sanitisedLeft
              (rightCaps, anyRight) <- $sanitisedRight
            } yield {
              val caps = leftCaps ++ rightCaps
              val any = Ordering[Option[Int]].max(anyLeft, anyRight)
              (caps, any)
            }
          }
          (expr, k)
        }
      }

      // TODO: Get rid of `asExprOf`.
      (expr.asExprOf[Option[(HChain[Concat[A, B]], Option[Int])]], j)
    }

    override def getType(using Quotes): Type[Concat[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[Concat[A, B]]
    }
  }

  private def empty(i: Int)(using Quotes): SanitiseCode[HNil] = {
    val expr = '{ (Some(HChain.nil, None)) }
    (expr, i)
  }
}
