package experiments.macros

import cats.collections.Diet
import cats.kernel.Order
import cats.syntax.all.*
import experiments.macros.evidence.{apply, liftCo}
import experiments.macros.hcollections.hchain.{HChain, HConcat, HEmpty, HCons, HSingleton, Tidy}
import parsley.templates.PureParserBridge0
import scala.quoted.{Expr, Quotes, Type}

object ast {

  type Groups = Array[Option[(String, Int)]]

  type OptionalCapture[A <: HChain] <: HChain = A match {
    case HEmpty => HEmpty
    case _      => SingleOption[A]
  }

  type SingleOption[A <: HChain] = HSingleton[Option[Tidy[A]]]

  type AltCapture[A <: HChain, B <: HChain] <: HChain = (A, B) match {
    case (HEmpty, HEmpty) => HEmpty
    case _                => SingleEither[A, B]
  }

  type SingleEither[A <: HChain, B <: HChain] = HSingleton[Either[Tidy[A], Tidy[B]]]

  case class Sanitised[+A <: HChain](captures: A, any: Option[Int])
  object Sanitised {
    given [A <: HChain] => Order[Sanitised[A]] = Order.by(_.any)
  }

  type SanitiseExpr[A <: HChain] = Expr[Option[Sanitised[A]]]
  case class SanitiseCode[A <: HChain](sanitised: SanitiseExpr[A], nextGroup: Int)

  sealed trait Regex[A <: HChain] {
    def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[A]

    def getType(using Quotes): Type[A]
  }

  sealed trait Match extends Regex[HEmpty] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HEmpty] = SanitiseCode(empty, i)
    override def getType(using Quotes): Type[HEmpty] = Type.of[HEmpty]
  }

  type Dot = Dot.type
  case object Dot extends Match with PureParserBridge0[Dot]
  case class Lit(c: Int) extends Match
  case class Class(cs: Diet[Int]) extends Match

  case class Capture[A <: HChain](inner: Regex[A]) extends Regex[HCons[String, A]] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HCons[String, A]] = {
      given Type[A] = inner.getType

      val idx = Expr(i)
      val SanitiseCode(sanitisedInner, j) = inner.sanitiseCode(groups, i + 1)
      val sanitised = '{
        $groups($idx).flatMap { (s, end) =>
          val innerCap = $sanitisedInner
          innerCap.map { case Sanitised(caps, _) =>
            Sanitised(s +: caps, Some(end))
          }
        }
      }
      SanitiseCode(sanitised, j)
    }

    override def getType(using Quotes): Type[HCons[String, A]] = {
      given Type[A] = inner.getType
      Type.of[HCons[String, A]]
    }
  }

  sealed trait CaptureInner[A <: HChain](inner: Regex[A]) extends Regex[A] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[A] = inner.sanitiseCode(groups, i)

    override def getType(using Quotes): Type[A] = inner.getType
  }

  case class NonCapture[A <: HChain](inner: Regex[A]) extends CaptureInner[A](inner)
  case class Rep1[A <: HChain](inner: Regex[A]) extends CaptureInner[A](inner)

  case class Alt[A <: HChain, B <: HChain](left: Regex[A], right: Regex[B]) extends Regex[AltCapture[A, B]] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[AltCapture[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      val sanitised = Expr.summon[HEmpty =:= AltCapture[A, B]].map { ev =>
        val sanitised = liftSanitised(ev)(empty)
        SanitiseCode(sanitised, i)
      } orElse Expr.summon[SingleEither[A, B] =:= AltCapture[A, B]].map { ev =>
        val SanitiseCode(sanitisedLeft, j) = left.sanitiseCode(groups, i)
        val SanitiseCode(sanitisedRight, k) = right.sanitiseCode(groups, j)
        val expr = liftSanitised(ev) {
          '{
            val left = $sanitisedLeft.map { case Sanitised(leftCaps, anyLeft) =>
              Sanitised(HChain.one(leftCaps.tidy.asLeft[Tidy[B]]), anyLeft)
            }
            val right = $sanitisedRight.map { case Sanitised(rightCaps, anyRight) =>
              Sanitised(HChain.one(rightCaps.tidy.asRight[Tidy[A]]), anyRight)
            }
            left max right
          }
        }
        SanitiseCode(expr, k)
      }

      // TODO: Any way to avoid `.get`
      sanitised.get
    }

    override def getType(using Quotes): Type[AltCapture[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[AltCapture[A, B]]
    }
  }

  sealed trait Optional[A <: HChain](inner: Regex[A]) extends Regex[OptionalCapture[A]] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[OptionalCapture[A]] = {      
      given Type[A] = inner.getType

      val sanitised = Expr.summon[HEmpty =:= OptionalCapture[A]].map { ev =>
        val sanitised = liftSanitised(ev)(empty)
        SanitiseCode(sanitised, i)
      } orElse Expr.summon[SingleOption[A] =:= OptionalCapture[A]].map { ev =>
        val SanitiseCode(sanitisedInner, j) = inner.sanitiseCode(groups, i)
        val sanitised = liftSanitised(ev) {
          '{
            val innerCaps = $sanitisedInner
            val innerAny = innerCaps.fold(None)(_.any)
            Some(Sanitised(HChain.one(innerCaps.map(_.captures.tidy)), innerAny))
          }
        }
        SanitiseCode(sanitised, j)
      }

      // TODO: Any way to avoid `.get`
      sanitised.get
    }

    override def getType(using Quotes): Type[OptionalCapture[A]] = {
      given Type[A] = inner.getType

      Type.of[OptionalCapture[A]]
    }
  }

  case class Opt[A <: HChain](inner: Regex[A]) extends Optional[A](inner)
  case class Rep0[A <: HChain](inner: Regex[A]) extends Optional[A](inner)

  case class Cat[A <: HChain, B <: HChain](left: Regex[A], right: Regex[B]) extends Regex[HConcat[A, B]] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HConcat[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Expr.summon[A =:= HConcat[A, B]].map { ev =>
        val SanitiseCode(sanitisedLeft, j) = left.sanitiseCode(groups, i)
        val sanitised = liftSanitised(ev)(sanitisedLeft)
        SanitiseCode(sanitised, j)
      } orElse Expr.summon[B =:= HConcat[A, B]].map { ev =>
        val SanitiseCode(sanitisedRight, j) = right.sanitiseCode(groups, i)
        val sanitised = liftSanitised(ev)(sanitisedRight)
        SanitiseCode(sanitised, j)
      } getOrElse {
        val SanitiseCode(sanitisedLeft, j) = left.sanitiseCode(groups, i)
        val SanitiseCode(sanitisedRight, k) = right.sanitiseCode(groups, j)
        val expr = '{
          for {
            Sanitised(leftCaps, anyLeft) <- $sanitisedLeft
            Sanitised(rightCaps, anyRight) <- $sanitisedRight
          } yield Sanitised(leftCaps ++ rightCaps, anyLeft max anyRight)
        }
        SanitiseCode(expr, k)
      }
    }

    override def getType(using Quotes): Type[HConcat[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[HConcat[A, B]]
    }
  }

  private def empty(using Quotes): SanitiseExpr[HEmpty] = {
    '{ (Some(Sanitised(HChain.nil, None))) }
  }

  private def liftSanitised[A <: HChain: Type, B <: HChain: Type](ev: Expr[A =:= B])(using Quotes): Expr[Option[Sanitised[A]] =:= Option[Sanitised[B]]] = {
    ev.liftCo[HChain, [X <: HChain] =>> Option[Sanitised[X]]]
  }
}
