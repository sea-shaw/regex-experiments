package experiments.macros.ast

import cats.syntax.all.*
import experiments.macros.hcollections.hchain.*
import experiments.macros.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

trait OptTypes { this: Functions =>
  sealed trait OptType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] { this: NodeType[G] =>
    final val asNodeType: NodeType[G] & OptType[F, G] = this
    def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using RepType[R])(using Quotes): SanitiseExpr[G[R]]
    def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?]
  }

  object OptType {
    def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F])(using Quotes): OptType[F, ?] = {
      inner.nodeType match {
        case _: HEmptyType => OptEmpty()
        case singletonOption: SingletonOption[f] => {
          given Type[f] = singletonOption.innerType
          OptNested(singletonOption)
        }
        case nonEmpty: HNonEmptyType[f] => {
          given Type[f] = nonEmpty.tpe
          OptSingleton(inner)
        }
      }
    }
  }

  /* A? */
  private class OptEmpty(using Type[Const[HEmpty]]) extends OptType[Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][R]])(using RepType[R])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
      flattenEmpty(nodes, types)
    }
  }

  /* (A)? */
  private type OptSingletonType = SingletonOptionType
  private class OptSingleton[F[_ <: Rep] <: HNonEmpty](inner: Tidiable[F])(using Type[F], Type[OptSingletonType[F]]) extends OptType[F, OptSingletonType[F]] with SingletonOption[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using RepType[R])(using Quotes): SanitiseExpr[OptSingletonType[F][R]] = {
      '{
        val innerCaps = $sanitisedInner
        SanitisedT(Some(innerCaps.value.sequence.map(HSingleton(_))))
      }
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[F[R]]], C], L, ?] = {
      flattenOpt(inner.tidyFunction, nodes, types)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = inner.tidyFunction
  }

  /* (A?)? */
  private type OptNestedType = SingletonOptionType
  private class OptNested[F[_ <: Rep] <: HNonEmpty](innerType: SingletonOption[F])(using Type[F], Type[OptNestedType[F]]) extends OptType[OptNestedType[F], OptNestedType[F]] with SingletonOption[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[OptNestedType[F][R]])(using RepType[R])(using Quotes): SanitiseExpr[OptNestedType[F][R]] = {
      sanitisedInner
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[F[R]]], C], L, ?] = {
      tidyInner match {
        case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
          case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[HSingleton[Option[F[R]]], C], L, b] {
            override def apply(chains: CCons[HSingleton[Option[F[R]]], C], leaves: L)(using Quotes): Expr[b] = {
              val opt = '{ ${ chains.head }.value.map(node => ${ tidy('node) }) }
              flatten(chains.tail, LCons(opt, leaves))
            }
          }
        }
      }
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = innerType.tidyInner
  }
}
