package experiments.macros.ast

import experiments.macros.hcollections.hchain.*
import experiments.macros.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

trait Rep0Types { this: Functions =>
  sealed trait Rep0Type[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] { this: NodeType[G] =>
    def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[G[R]]
    def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?]
  }

  private class Rep0Empty(using Type[Const[HEmpty]]) extends Rep0Type[Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][true]])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
      flattenEmpty(nodes, types)
    }
  }

  private type Rep0OptType[F[_ <: Rep] <: HNonEmpty] = SingletonOptionType[Const[F[true]]]
  private class Rep0Opt[F[_ <: Rep] <: HNonEmpty: Type](innerType: SingletonOption[F])(using Type[Const[F[true]]], Type[Rep0OptType[F]]) extends Rep0Type[SingletonOptionType[F], Rep0OptType[F]] with SingletonOption[Const[F[true]]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[SingletonOptionType[F][true]])(using Quotes): SanitiseExpr[Rep0OptType[F][R]] = {
      sanitisedInner
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[F[true]]], C], L, ?] = {
      tidyInner match {
        case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
          case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[HSingleton[Option[F[true]]], C], L, b] {
            override def apply(chains: CCons[HSingleton[Option[F[true]]], C], leaves: L)(using Quotes): Expr[b] = {
              val opt = '{ ${ chains.head }.value.map(node => ${ tidy('node) }) }
              flatten(chains.tail, LCons(opt, leaves))
            }
          }
        }
      }
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[true], ?] = innerType.tidyInner(using RepTrue)
  }

  private type Rep0NonEmptyType[F[_ <: Rep] <: HNonEmpty] = SingletonOptionType[Const[F[true]]]
  private class Rep0NonEmpty[F[_ <: Rep] <: HNonEmpty: Type](inner: Tidiable[F])(using Type[Const[F[true]]], Type[Rep0NonEmptyType[F]]) extends Rep0Type[F, Rep0NonEmptyType[F]] with SingletonOption[Const[F[true]]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[Rep0NonEmptyType[F][R]] = {
      sanitiseOpt(sanitisedInner)
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[F[true]]], C], L, ?] = {
      flattenOpt(inner.tidyFunction(using RepTrue), nodes, types)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[true], ?] = inner.tidyFunction(using RepTrue)
  }

  case class Rep0TypeRes[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](value: NodeType[G] & Rep0Type[F, G])
  object Rep0Type {
    def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F])(using Quotes): Rep0TypeRes[F, ?] = {
      given Type[F] = inner.nodeType.tpe
      inner.nodeType match {
        case _: HEmptyType => Rep0TypeRes(Rep0Empty())
        case option: SingletonOption[f] => {
          given Type[f] = option.innerType
          Rep0TypeRes(Rep0Opt(option))
        }
        case _: HNonEmptyType[_] => Rep0TypeRes(Rep0NonEmpty(inner))
      }
    }
  }
}
