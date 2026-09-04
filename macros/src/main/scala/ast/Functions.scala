package experiments.macros.ast

import experiments.macros.hcollections.hchain.HChain
import scala.quoted.{Expr, Type, Quotes}

type Groups = Array[Option[String]]

type Rep = Boolean
sealed trait RepType[R <: Rep]
case object RepTrue extends RepType[true]
case object RepFalse extends RepType[false]

type Id[+A] = A
type Const[+A] = [_] =>> A

trait Functions {
  abstract class Tidiable[F[_ <: Rep] <: HChain] {
    final def tidyFunction[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = {
      flattenFunction(NNil, TNil) match {
        case flatten @ FlattenFunction(given Type[a]) => new TidyFunction[F[R], a] {
          override def apply(chain: Expr[F[R]])(using Quotes): Expr[a] = {
            flatten(CCons(chain, CNil), LNil)
          }
        }
      }
    }

    def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?]
  }

  protected sealed trait Nodes[C <: Chains] {
    def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[C, L, ?]
  }

  protected case object NNil extends Nodes[CNil] {
    override def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[CNil, L, ?] = {
      buildFunction(types) match {
        case build @ BuildFunction(given Type[a]) => new FlattenFunction[CNil, L, a] {
          override def apply(chains: CNil, leaves: L)(using Quotes): Expr[a] = {
            build(leaves)
          }
        }
      }
    }
  }

  protected case class NCons[F[_ <: Rep] <: HChain, R <: Rep: Type, C <: Chains](head: Tidiable[F], rep: RepType[R], tail: Nodes[C]) extends Nodes[CCons[F[R], C]] {
    override def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
      given RepType[R] = rep
      head.flattenFunction(tail, types)
    }
  }

  protected sealed trait Chains
  protected type CNil = CNil.type
  protected case object CNil extends Chains
  protected case class CCons[A <: HChain, C <: Chains](head: Expr[A], tail: C) extends Chains

  protected sealed trait Types[L <: Leaves]
  protected case object TNil extends Types[LNil]
  protected case class TCons[A, L <: Leaves](head: Type[A], tail: Types[L]) extends Types[LCons[A, L]]

  protected sealed trait Leaves
  protected type LNil = LNil.type
  protected case object LNil extends Leaves
  protected case class LCons[A, L <: Leaves](head: Expr[A], tail: L) extends Leaves

  abstract class TidyFunction[A <: HChain, B](using val tpe: Type[B]) {
    def apply(chain: Expr[A])(using Quotes): Expr[B]
  }

  object TidyFunction {
    def unapply[A <: HChain, B](tidyFunction: TidyFunction[A, B]): Tuple1[Type[B]] = Tuple1(tidyFunction.tpe)
  }

  protected abstract class FlattenFunction[C <: Chains, L <: Leaves, A](using val tpe: Type[A]) {
    def apply(chains: C, leaves: L)(using Quotes): Expr[A]
  }

  protected object FlattenFunction {
    def unapply[C <: Chains, L <: Leaves, A](tidyFunction: FlattenFunction[C, L, A]): Tuple1[Type[A]] = Tuple1(tidyFunction.tpe)
  }

  protected abstract class BuildFunction[L <: Leaves, A](using val tpe: Type[A]) {
    def apply(leaves: L)(using Quotes): Expr[A]
  }

  protected object BuildFunction {
    def unapply[L <: Leaves, A](buildFunction: BuildFunction[L, A]): Tuple1[Type[A]] = Tuple1(buildFunction.tpe)
  }

  protected def buildFunction[L <: Leaves](types: Types[L])(using Quotes): BuildFunction[L, ?]
}
