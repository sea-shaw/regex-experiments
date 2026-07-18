package experiments.macros.alt

import experiments.macros.evidence.{apply, liftCo}
import experiments.macros.hcollections.hchain.{HChain, HConcat, HCons, HEmpty, HSingleton, Tidy}
import cats.collections.Diet
import cats.data.Ior
// import cats.data.Ior.{Both => IBoth, Left => ILeft, Right => IRight}
import cats.kernel.Order
import cats.syntax.all.*
import scala.quoted.{Expr, Quotes, Type}

object ast {

  type Rep = Boolean & Singleton

  type Const[A] = [_] =>> A

  type Groups = Array[Option[String]]

  case class Sanitised[+A <: HChain](captures: A, any: Boolean)
  object Sanitised {
    given [A <: HChain] => Order[Sanitised[A]] = Order.by(_.any)
  }

  type SanitiseExpr[+A <: HChain] = Expr[Option[Sanitised[A]]]
  case class SanitiseCode[+A <: HChain](sanitised: SanitiseExpr[A], nextGroup: Int)

  sealed trait Regex[F[_ <: Rep] <: HChain] {
    def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[F[R]]

    def getType(using Quotes): Type[F]
  }

  sealed trait Match extends Regex[Const[HEmpty]] {
    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HEmpty] = {
      SanitiseCode(empty, i)
    }
    override def getType(using Quotes): Type[Const[HEmpty]] = {
      Type.of[Const[HEmpty]]
    }
  }

  case object Dot extends Match
  case class Lit(c: Int) extends Match
  case class Class(cs: Diet[Int]) extends Match

  type CaptureType[F[_ <: Rep] <: HChain] = [R <: Rep] =>> HCons[String, F[R]]
  case class Capture[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Regex[CaptureType[F]] {
    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HCons[String, F[R]]] = {
      given Type[F] = inner.getType

      val idx = Expr(i)
      val SanitiseCode(sanitisedInner, j) = inner.sanitiseCode(groups, i + 1)
      val sanitised = '{
        $groups($idx).flatMap { s =>
          val innerCap = $sanitisedInner
          innerCap.map { case Sanitised(caps, _) =>
            Sanitised(s +: caps, true)
          }
        }
      }
      SanitiseCode(sanitised, j)
    }

    override def getType(using Quotes): Type[CaptureType[F]] = {
      given Type[F] = inner.getType
      Type.of[CaptureType[F]]
    }
  }

  case class NonCapture[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Regex[F] {
    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[F[R]] = {
      inner.sanitiseCode(groups, i)
    }
    override def getType(using Quotes): Type[F] = {
      inner.getType
    }
  }

  type OptType[F[_ <: Rep] <: HChain] = [R <: Rep] =>> OptCapture[F[R]] 
  type OptCapture[A <: HChain] <: HChain = A match {
    case HEmpty => HEmpty
    case _      => HSingleton[Option[Tidy[A]]]
  }

  case class Opt[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Regex[OptType[F]] {
    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[OptType[F][R]] = {
      given Type[F] = inner.getType

      val sanitised = Expr.summon[HEmpty =:= OptType[F][R]].map { ev =>
        val sanitised = liftSanitised(ev)(empty)
        SanitiseCode(sanitised, i)
      } orElse Expr.summon[HSingleton[Option[Tidy[F[R]]]] =:= OptType[F][R]].map { ev =>
        val SanitiseCode(sanitisedInner, j) = inner.sanitiseCode(groups, i)
        val sanitised = liftSanitised(ev) {
          '{
            val innerCaps = $sanitisedInner
            val innerAny = innerCaps.fold(false)(_.any)
            Some(Sanitised(HChain.one(innerCaps.map(_.captures.tidy)), innerAny))
          }
        }
        SanitiseCode(sanitised, j)
      }

      sanitised.get
    }
    override def getType(using Quotes): Type[OptType[F]] = {
      given Type[F] = inner.getType

      Type.of[OptType[F]]
    }
  }

  type CatType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] = [R <: Rep] =>> HConcat[F[R], G[R]]
  case class Cat[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G]) extends Regex[CatType[F, G]] {
    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[CatType[F, G][R]] = {
      given Type[F] = left.getType
      given Type[G] = right.getType

      Expr.summon[F[R] =:= CatType[F, G][R]].map { ev =>
        val SanitiseCode(sanitisedLeft, j) = left.sanitiseCode(groups, i)
        val sanitised = liftSanitised(ev)(sanitisedLeft)
        SanitiseCode(sanitised, j)
      } orElse Expr.summon[G[R] =:= CatType[F, G][R]].map { ev =>
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
          } yield Sanitised(leftCaps ++ rightCaps, anyLeft || anyRight)
        }
        SanitiseCode(expr, k)
      }
    }

    override def getType(using Quotes): Type[CatType[F, G]] = {
      given Type[F] = left.getType
      given Type[G] = right.getType

      Type.of[CatType[F, G]]
    }
  }

  type AltType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] = [R <: Rep] =>> AltCapture[F[R], G[R], R]
  type AltCapture[A <: HChain, B <: HChain, R <: Rep] <: HChain = (A, B) match {
    case (HEmpty, HEmpty) => HEmpty
    case _                => R match {
      case true  => SingletonIor[A, B]
      case false => SingletonEither[A, B]
    }
  }

  type SingletonIor[A <: HChain, B <: HChain] = SingletonWith[Ior, A, B]
  type SingletonEither[A <: HChain, B <: HChain] = SingletonWith[Either, A, B]
  type SingletonWith[F[_, _], A <: HChain, B <: HChain] = HSingleton[F[Tidy[A], Tidy[B]]]

  case class Alt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G]) extends Regex[AltType[F, G]] {
    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[AltType[F, G][R]] = {
      given Type[F] = left.getType
      given Type[G] = right.getType

      val sanitised = Expr.summon[HEmpty =:= AltType[F, G][R]].map { ev =>
        val sanitised = liftSanitised(ev)(empty)
        SanitiseCode(sanitised, i)
      } orElse Expr.summon[SingletonEither[F[R], G[R]] =:= AltType[F, G][R]].map { ev =>
        val SanitiseCode(sanitisedLeft, j) = left.sanitiseCode(groups, i)
        val SanitiseCode(sanitisedRight, k) = right.sanitiseCode(groups, j)
        val expr = liftSanitised(ev) {
          '{
            val left = $sanitisedLeft.map { case Sanitised(leftCaps, anyLeft) =>
              Sanitised(HChain.one(leftCaps.tidy.asLeft[Tidy[G[R]]]), anyLeft)
            }
            val right = $sanitisedRight.map { case Sanitised(rightCaps, anyRight) =>
              Sanitised(HChain.one(rightCaps.tidy.asRight[Tidy[F[R]]]), anyRight)
            }
            left max right
          }
        }
        SanitiseCode(expr, k)
      } orElse Expr.summon[SingletonIor[F[R], G[R]] =:= AltType[F, G][R]].map { ev =>
        val SanitiseCode(sanitisedLeft, j) = left.sanitiseCode(groups, i)
        val SanitiseCode(sanitisedRight, k) = right.sanitiseCode(groups, j)
        val expr = liftSanitised(ev) {
          ???
        }
        SanitiseCode(expr, k)
      }

      sanitised.get
    }

    override def getType(using Quotes): Type[AltType[F, G]] = {
      given Type[F] = left.getType
      given Type[G] = right.getType

      Type.of[AltType[F, G]]
    }
  }

  object Rep0 {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F]) = Opt(Rep1(inner))
  }

  /*

  type Rep0Type[F[_ <: Rep] <: HChain] = OptType[Rep1Type[F]]
  class Rep0[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Regex[Rep0Type[F]] {
    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[Rep0Type[F][R]] = {
      Opt(Rep1(inner)).sanitiseCode(groups, i)
    }

    override def getType(using Quotes): Type[Rep0Type[F]] = {
      given Type[F] = inner.getType

      Type.of[Rep0Type[F]]
    }
  }

  */

  type Rep1Type[F[_ <: Rep] <: HChain] = Const[F[true]]
  case class Rep1[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Regex[Rep1Type[F]] {
    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[Rep1Type[F][R]] = {
      inner.sanitiseCode(groups, i)
    }

    override def getType(using Quotes): Type[Rep1Type[F]] = {
      given Type[F] = inner.getType

      Type.of[Rep1Type[F]]
    }
  }

  private def empty(using Quotes): SanitiseExpr[HEmpty] = {
    '{ (Some(Sanitised(HChain.nil, false))) }
  }

  private def liftSanitised[A <: HChain: Type, B <: HChain: Type](ev: Expr[A =:= B])(using Quotes): Expr[Option[Sanitised[A]] =:= Option[Sanitised[B]]] = {
    ev.liftCo[HChain, [X <: HChain] =>> Option[Sanitised[X]]]
  }
}
