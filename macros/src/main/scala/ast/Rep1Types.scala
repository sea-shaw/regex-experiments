package experiments.macros.ast

import experiments.macros.hcollections.hchain.*
import experiments.macros.sanitised.*
import scala.quoted.{Quotes, Type}

trait Rep1Types { this: Functions =>
  sealed trait Rep1Type[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] { this: NodeType[G] =>
    def sanitiseCode[R <: Rep](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[G[R]]
    def flattenFunction[C <: Chains, L <: Leaves, R <: Rep](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?]
  }

  case class Rep1TypeRes[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](value: NodeType[G] & Rep1Type[F, G])
  object Rep1Type {
    def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F])(using Quotes): Rep1TypeRes[F, ?] = {
      given Type[F] = inner.nodeType.tpe
      inner.nodeType match {
        case _: HEmptyType       => Rep1TypeRes(Rep1Empty())
        case _: HNonEmptyType[_] => Rep1TypeRes(Rep1NonEmpty(inner))
      }
    }
  }

  private class Rep1Empty(using Type[Const[HEmpty]]) extends Rep1Type[Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep](sanitisedInner: => SanitiseExpr[Const[HEmpty][true]])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
      flattenEmpty(nodes, types)
    }
  }

  private type Rep1NonEmptyType[F[_ <: Rep] <: HNonEmpty] = Const[F[true]]
  private class Rep1NonEmpty[F[_ <: Rep] <: HNonEmpty](inner: Tidiable[F])(using Type[Rep1NonEmptyType[F]]) extends Rep1Type[F, Rep1NonEmptyType[F]] with HNonEmptyType[Rep1NonEmptyType[F]] {
    override def sanitiseCode[R <: Rep](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[Rep1NonEmptyType[F][R]] = {
      sanitisedInner
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[F[true], C], L, ?] = {
      inner.flattenFunction(nodes, types)(using RepTrue)
    }
  }
}
