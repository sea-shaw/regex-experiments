package experiments.macros

import cats.collections.Diet
import cats.kernel.Order
import cats.syntax.all.*
import experiments.macros.hchain.{HChain, HConcat, HEmpty, HPrepended, HSingleton, Tidy}
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
  type SanitiseCode[A <: HChain] = (SanitiseExpr[A], Int)

  sealed trait Regex[A <: HChain] {
    def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[A]

    // TODO: Make this a `val` somehow to avoid traversing all children at each
    // node. Move `Quotes` into constructor?
    def getType(using Quotes): Type[A]
  }

  sealed trait Match extends Regex[HEmpty] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HEmpty] = empty(i)
    override def getType(using Quotes): Type[HEmpty] = Type.of[HEmpty]
  }

  type Dot = Dot.type
  case object Dot extends Match with PureParserBridge0[Dot]
  case class Lit(c: Int) extends Match
  case class Class(cs: Diet[Int]) extends Match

  case class Capture[A <: HChain](inner: Regex[A]) extends Regex[HPrepended[String, A]] {
    override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HPrepended[String, A]] = {
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

    override def getType(using Quotes): Type[HPrepended[String, A]] = {
      given Type[A] = inner.getType
      Type.of[HPrepended[String, A]]
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
        val sanitised = '{ Some(Sanitised($ev(HChain.nil), None)) }
        (sanitised, i)
      } orElse Expr.summon[SingleEither[A, B] =:= AltCapture[A, B]].map { ev =>
        val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
        val (sanitisedRight, k) = right.sanitiseCode(groups, j)
        val expr = '{
          val left = $sanitisedLeft.map { case Sanitised(leftCaps, anyLeft) =>
            Sanitised($ev(HChain.one(leftCaps.tidy.asLeft[Tidy[B]])), anyLeft)
          }
          val right = $sanitisedRight.map { case Sanitised(rightCaps, anyRight) =>
            Sanitised($ev(HChain.one(rightCaps.tidy.asRight[Tidy[A]])), anyRight)
          }
          left max right
        }
        (expr, k)
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
        val sanitised = '{ Some(Sanitised($ev(HChain.nil), None)) }
        (sanitised, i)
      } orElse Expr.summon[SingleOption[A] =:= OptionalCapture[A]].map { ev =>
        val (sanitisedInner, j) = inner.sanitiseCode(groups, i)
        val sanitised = '{
          val innerCaps = $sanitisedInner
          val innerAny = innerCaps.fold(None)(_.any)
          Some(Sanitised($ev(HChain.one(innerCaps.map(_.captures.tidy))), innerAny))
        }
        (sanitised, j)
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
        val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
        val sanitised = '{
          $sanitisedLeft.map { case Sanitised(captures, any) =>
            Sanitised($ev(captures), any)
          }
        }
        (sanitised, j)
      } orElse Expr.summon[B =:= HConcat[A, B]].map { ev =>
        val (sanitisedRight, j) = right.sanitiseCode(groups, i)
        val sanitised = '{
          $sanitisedRight.map { case Sanitised(captures, any) => 
            Sanitised($ev(captures), any)
          }
        }
        (sanitised, j)
      } getOrElse {
        val (sanitisedLeft, j) = left.sanitiseCode(groups, i)
        val (sanitisedRight, k) = right.sanitiseCode(groups, j)
        val expr: SanitiseExpr[HConcat[A, B]] = '{
          for {
            Sanitised(leftCaps, anyLeft) <- $sanitisedLeft
            Sanitised(rightCaps, anyRight) <- $sanitisedRight
          } yield Sanitised(leftCaps ++ rightCaps, anyLeft max anyRight)
        }
        (expr, k)
      }
    }

    override def getType(using Quotes): Type[HConcat[A, B]] = {
      given Type[A] = left.getType
      given Type[B] = right.getType

      Type.of[HConcat[A, B]]
    }
  }

  private def empty(i: Int)(using Quotes): SanitiseCode[HEmpty] = {
    val expr = '{ (Some(Sanitised(HChain.nil, None))) }
    (expr, i)
  }
}
