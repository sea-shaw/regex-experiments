package experiments.macros.alt

import experiments.macros.hcollections.hchain.{HChain, HConcat, HCons, HEmpty, HSingleton, Tidy}
import cats.collections.Diet
import cats.data.Ior
// import cats.data.Ior.{Both => IBoth, Left => ILeft, Right => IRight}
import cats.kernel.Order
import scala.quoted.{Expr, Quotes, Type}

object altast {

  type Const[A] = [_] =>> A

  type Groups = Array[Option[(String, Int)]]

  case class Sanitised[+A <: HChain](captures: A, any: Boolean)
  object Sanitised {
    given [A <: HChain] => Order[Sanitised[A]] = Order.by(_.any)
  }

  type SanitiseExpr[A <: HChain] = Expr[Option[Sanitised[A]]]
  case class SanitiseCode[A <: HChain](sanitised: SanitiseExpr[A], nextGroup: Int)

  sealed trait Regex[F[_ <: Boolean] <: HChain] {
    def sanitiseCode[R <: Boolean](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[F[R]]

    def getType(using Quotes): Type[F]
  }

  sealed trait Match extends Regex[Const[HEmpty]] {
    override def sanitiseCode[R <: Boolean](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HEmpty] = ???
    override def getType(using Quotes): Type[Const[HEmpty]] = Type.of[Const[HEmpty]]
  }

  case object Dot extends Match
  case class Lit(c: Int) extends Match
  case class Class(cs: Diet[Int]) extends Match

  type CaptureType[F[_ <: Boolean] <: HChain] = [R <: Boolean] =>> HCons[String, F[R]]
  case class Capture[F[_ <: Boolean] <: HChain](inner: Regex[F]) extends Regex[CaptureType[F]] {
    override def sanitiseCode[R <: Boolean](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HCons[String, F[R]]] = ???
    override def getType(using Quotes): Type[CaptureType[F]] = {
      given Type[F] = inner.getType
      Type.of[CaptureType[F]]
    }
  }

  case class NonCapture[F[_ <: Boolean] <: HChain](inner: Regex[F]) extends Regex[F] {
    override def sanitiseCode[R <: Boolean](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[F[R]] = ???
    override def getType(using Quotes): Type[F] = inner.getType
  }

  type OptType[F[_ <: Boolean] <: HChain] = [R <: Boolean] =>> HSingleton[Option[Tidy[F[R]]]] 
  case class Opt[F[_ <: Boolean] <: HChain](inner: Regex[F]) extends Regex[OptType[F]] {
    override def sanitiseCode[R <: Boolean](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[OptType[F][R]] = ???
    override def getType(using Quotes): Type[OptType[F]] = {
      given Type[F] = inner.getType
      
      Type.of[OptType[F]]
    }
  }

  type CatType[F[_ <: Boolean] <: HChain, G[_ <: Boolean] <: HChain] = [R <: Boolean] =>> HConcat[F[R], G[R]]
  case class Cat[F[_ <: Boolean] <: HChain, G[_ <: Boolean] <: HChain](left: Regex[F], right: Regex[G]) extends Regex[CatType[F, G]] {
    override def sanitiseCode[R <: Boolean](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[CatType[F, G][R]] = ???
    override def getType(using Quotes): Type[CatType[F, G]] = {
      given Type[F] = left.getType
      given Type[G] = right.getType

      Type.of[CatType[F, G]]
    }
  }

  type AltRep[F[_ <: Boolean] <: HChain, G[_ <: Boolean] <: HChain, R <: Boolean] <: HChain = R match {
    case true  => HSingleton[Ior[Tidy[F[R]], Tidy[G[R]]]]
    case false => HSingleton[Either[Tidy[F[R]], Tidy[G[R]]]]
  }

  type AltType[F[_ <: Boolean] <: HChain, G[_ <: Boolean] <: HChain] = [R <: Boolean] =>> AltRep[F, G, R]
  case class Alt[F[_ <: Boolean] <: HChain, G[_ <: Boolean] <: HChain](left: Regex[F], right: Regex[G]) extends Regex[AltType[F, G]] {
    override def sanitiseCode[R <: Boolean](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[AltType[F, G][R]] = ???

    override def getType(using Quotes): Type[AltType[F, G]] = {
      given Type[F] = left.getType
      given Type[G] = right.getType

      Type.of[AltType[F, G]]
    }
  }

  type Rep0Type[F[_ <: Boolean] <: HChain] = Const[HSingleton[Option[Tidy[F[true]]]]]
  case class Rep0[F[_ <: Boolean] <: HChain](inner: Regex[F]) extends Regex[Rep0Type[F]] {
    override def sanitiseCode[R <: Boolean](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[Rep0Type[F][R]] = ???

    override def getType(using Quotes): Type[Rep0Type[F]] = {
      given Type[F] = inner.getType

      Type.of[Rep0Type[F]]
    }
  }

  type Rep1Type[F[_ <: Boolean] <: HChain] = Const[F[true]]
  case class Rep1[F[_ <: Boolean] <: HChain](inner: Regex[F]) extends Regex[Rep1Type[F]] {
    override def sanitiseCode[R <: Boolean](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[Rep1Type[F][R]] = {
      inner.sanitiseCode[true](groups, i)
    }

    override def getType(using Quotes): Type[Rep1Type[F]] = {
      given Type[F] = inner.getType

      Type.of[Rep1Type[F]]
    }
  }
}
