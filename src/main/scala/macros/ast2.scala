package experiments.macros

import experiments.macros.hchain.{HChain, HConcat, HEmpty, HPrepended, HSingleton, Tidy}
import cats.collections.Diet
import cats.data.Ior
import cats.kernel.Order
import scala.quoted.{Expr, Quotes, Type}

object ast2 {

  sealed trait RepStatus

  sealed trait NoRep extends RepStatus
  case object NoRep extends NoRep

  sealed trait SomeRep extends RepStatus
  case object SomeRep extends SomeRep

  sealed trait ManyRep extends RepStatus
  case object ManyRep extends ManyRep

  type SomeRepAnd[S <: RepStatus] = S match {
    case ManyRep => ManyRep
    case _       => SomeRep
  }

  type AltRep[A, B, S <: RepStatus] <: HChain = S match {
    case ManyRep => HConcat[HSingleton[Option[A]], HSingleton[Option[B]]]
    case SomeRep => HSingleton[Ior[A, B]]
    case NoRep   => HSingleton[Either[A, B]]
  }

  type Const[A] = [_] =>> A

  type Groups = Array[Option[(String, Int)]]

  case class Sanitised[+A <: HChain](captures: A, any: Boolean)
  object Sanitised {
    given [A <: HChain] => Order[Sanitised[A]] = Order.by(_.any)
  }

  type SanitiseExpr[A <: HChain] = Expr[Option[Sanitised[A]]]
  case class SanitiseCode[A <: HChain](sanitised: SanitiseExpr[A], nextGroup: Int)

  sealed trait Regex[F[_ <: RepStatus] <: HChain] {
    def sanitiseCode[S <: RepStatus](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[F[S]]

    def getType(using Quotes): Type[F]
  }

  sealed trait Match extends Regex[Const[HEmpty]] {
    override def sanitiseCode[S <: RepStatus](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HEmpty] = ???
    override def getType(using Quotes): Type[Const[HEmpty]] = Type.of[Const[HEmpty]]
  }

  case object Dot extends Match
  case class Lit(c: Int) extends Match
  case class Class(cs: Diet[Int]) extends Match

  type CaptureType[F[_ <: RepStatus] <: HChain] = [S <: RepStatus] =>> HPrepended[String, F[S]]
  case class Capture[F[_ <: RepStatus] <: HChain](inner: Regex[F]) extends Regex[CaptureType[F]] {
    override def sanitiseCode[S <: RepStatus](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[HPrepended[String, F[S]]] = ???
    override def getType(using Quotes): Type[CaptureType[F]] = {
      given Type[F] = inner.getType
      Type.of[CaptureType[F]]
    }
  }

  case class NonCapture[F[_ <: RepStatus] <: HChain](inner: Regex[F]) extends Regex[F] {
    override def sanitiseCode[S <: RepStatus](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[F[S]] = ???
    override def getType(using Quotes): Type[F] = inner.getType
  }

  type OptType[F[_ <: RepStatus] <: HChain] = [S <: RepStatus] =>> HSingleton[Option[F[S]]] 
  case class Opt[F[_ <: RepStatus] <: HChain](inner: Regex[F]) extends Regex[OptType[F]] {
    override def sanitiseCode[S <: RepStatus](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[OptType[F][S]] = ???
    override def getType(using Quotes): Type[OptType[F]] = {
      given Type[F] = inner.getType
      
      Type.of[OptType[F]]
    }
  }

  type CatType[F[_ <: RepStatus] <: HChain, G[_ <: RepStatus] <: HChain] = [S <: RepStatus] =>> HConcat[F[S], G[S]]
  case class Cat[F[_ <: RepStatus] <: HChain, G[_ <: RepStatus] <: HChain](left: Regex[F], right: Regex[G]) extends Regex[CatType[F, G]] {
    override def sanitiseCode[S <: RepStatus](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[CatType[F, G][S]] = ???
    override def getType(using Quotes): Type[CatType[F, G]] = {
      given Type[F] = left.getType
      given Type[G] = right.getType

      Type.of[CatType[F, G]]
    }
  }

  type AltType[F[_ <: RepStatus] <: HChain, G[_ <: RepStatus] <: HChain] = [S <: RepStatus] =>> AltRep[F[S], G[S], S]
  case class Alt[F[_ <: RepStatus] <: HChain, G[_ <: RepStatus] <: HChain](left: Regex[F], right: Regex[G]) extends Regex[AltType[F, G]] {
    override def sanitiseCode[S <: RepStatus](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[AltType[F, G][S]] = ???

    override def getType(using Quotes): Type[AltType[F, G]] = {
      given Type[F] = left.getType
      given Type[G] = right.getType

      Type.of[AltType[F, G]]
    }
  }

  type Rep0Type[F[_ <: RepStatus] <: HChain] = Const[F[ManyRep]]
  case class Rep0[F[_ <: RepStatus] <: HChain](inner: Regex[F]) extends Regex[Rep0Type[F]] {
    override def sanitiseCode[S <: RepStatus](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[Rep0Type[F][S]] = ???

    override def getType(using Quotes): Type[Rep0Type[F]] = {
      given Type[F] = inner.getType

      Type.of[Rep0Type[F]]
    }
  }

  type Rep1Type[F[_ <: RepStatus] <: HChain] = [S <: RepStatus] =>> F[SomeRepAnd[S]]
  case class Rep1[F[_ <: RepStatus] <: HChain](inner: Regex[F]) extends Regex[Rep1Type[F]] {
    override def sanitiseCode[S <: RepStatus](groups: Expr[Groups], i: Int)(using Quotes): SanitiseCode[Rep1Type[F][S]] = ???

    override def getType(using Quotes): Type[Rep1Type[F]] = {
      given Type[F] = inner.getType

      Type.of[Rep1Type[F]]
    }
  }
}
