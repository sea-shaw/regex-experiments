package experiments.macros.ast

import cats.syntax.all.*
import experiments.macros.hcollections.hchain.*
import experiments.macros.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

trait CatTypes { this: Functions =>
  sealed trait CatType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] { this: NodeType[H] =>
    def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[G[R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[H[R]]
    def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[H[R], C], L, ?]
  }

  case class CatTypeRes[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain](value: NodeType[H] & CatType[F, G, H])
  object CatType {
    def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Tidiable[F], right: Tidiable[G])(using Quotes): CatTypeRes[F, G, ?] = {
      (left.nodeType, right.nodeType) match {
        case (_: HEmptyType, _: HEmptyType) => CatTypeRes(CatEmpty())
        case (leftType: SingletonOption[f], _: HEmptyType) => {
          given Type[f] = leftType.innerType
          CatTypeRes(CatLeftOption(leftType))
        }
        case (_: HEmptyType, rightType: SingletonOption[f]) => {
          given Type[f] = rightType.innerType
          CatTypeRes(CatRightOption(rightType))
        }
        case (leftType: HNonEmptyType[f], _: HEmptyType) => {
          given Type[f] = leftType.tpe
          CatTypeRes(CatLeft(left))
        }
        case (_: HEmptyType, rightType: HNonEmptyType[g]) => {
          given Type[g] = rightType.tpe
          CatTypeRes(CatRight(right))
        }
        case (leftType: HNonEmptyType[f], rightType: HNonEmptyType[g]) => {
          given Type[f] = leftType.tpe
          given Type[g] = rightType.tpe
          CatTypeRes(CatBoth(left, right))
        }
      }
    }
  }

  private class CatEmpty(using Type[Const[HEmpty]]) extends CatType[Const[HEmpty], Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
      flattenEmpty(nodes, types)
    }
  }

  private class CatLeftOption[F[_ <: Rep] <: HNonEmpty](leftType: SingletonOption[F])(using Type[F], Type[SingletonOptionType[F]]) extends CatType[SingletonOptionType[F], Const[HEmpty], SingletonOptionType[F]] with SingletonOption[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[SingletonOptionType[F][R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[SingletonOptionType[F][R]] = {
      sanitisedLeft
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[SingletonOptionType[F][R], C], L, ?] = {
      flattenOpt(tidyInner, nodes, types)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = leftType.tidyInner
  }

  private class CatRightOption[G[_ <: Rep] <: HNonEmpty](rightType: SingletonOption[G])(using Type[G], Type[SingletonOptionType[G]]) extends CatType[Const[HEmpty], SingletonOptionType[G], SingletonOptionType[G]] with SingletonOption[G] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[SingletonOptionType[G][R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[SingletonOptionType[G][R]] = {
      sanitisedRight
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[G[R]]], C], L, ?] = {
      flattenOpt(tidyInner, nodes, types)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[G[R], ?] = rightType.tidyInner
  }

  private class CatLeft[F[_ <: Rep] <: HNonEmpty](left: Tidiable[F])(using Type[F]) extends CatType[F, Const[HEmpty], F] with HNonEmptyType[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[F[R]] = {
      sanitisedLeft
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
      left.flattenFunction(nodes, types)
    }
  }

  private class CatRight[G[_ <: Rep] <: HNonEmpty](right: Tidiable[G])(using Type[G]) extends CatType[Const[HEmpty], G, G] with HNonEmptyType[G] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[G[R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[G[R]] = {
      sanitisedRight
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
      right.flattenFunction(nodes, types)
    }
  }

  private type CatBothType[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HAppend[F[R], G[R]]
  private class CatBoth[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type](left: Tidiable[F], right: Tidiable[G])(using Type[CatBothType[F, G]]) extends CatType[F, G, CatBothType[F, G]] with HNonEmptyType[CatBothType[F, G]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[G[R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[CatBothType[F, G][R]] = {
      '{
        for {
          left <- $sanitisedLeft
          right <- $sanitisedRight
        } yield HAppend(left, right)
      }
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using rep: RepType[R])(using Quotes): FlattenFunction[CCons[HAppend[F[R], G[R]], C], L, ?] = {
      left.flattenFunction(NCons(right, rep, nodes), types) match {
        case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HAppend[F[R], G[R]], C], L, a] {
          override def apply(chains: CCons[HAppend[F[R], G[R]], C], leaves: L)(using Quotes): Expr[a] = {
            '{
              val node = ${ chains.head }
              ${ flatten(CCons('{ node.left }, CCons('{ node.right }, chains.tail)), leaves) }
            }
          }
        }
      }
    }
  }
}
