package experiments.macros.ast

import cats.syntax.all.*
import experiments.macros.hcollections.hchain.*
import experiments.macros.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

trait CapturingTypes { this: Functions =>
  sealed trait CapturingType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] { this: NodeType[G] =>
    def sanitiseCode[R <: Rep: Type](sanitisedCapture: Expr[SanitisedT[Option, HSingleton[String]]], sanitisedInner: => Expr[SanitisedT[Option, F[R]]])(using Quotes): Expr[SanitisedT[Option, G[R]]]
    def flattenFunction[R <: Rep: Type, C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes, RepType[R]): FlattenFunction[CCons[G[R], C], L, ?]
  }

  case class CapturingTypeRes[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](value: NodeType[G] & CapturingType[F, G])
  object CapturingType {
    def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F])(using Quotes): CapturingTypeRes[F, ?] = {
      given Type[F] = inner.nodeType.tpe
      inner.nodeType match {
        case _: HEmptyType       => CapturingTypeRes(CapturingSingleton())
        case _: HNonEmptyType[_] => CapturingTypeRes(CapturingAppend(inner))
      }
    }
  }

  private type CapturingSingletonType = Const[HSingleton[String]]
  private class CapturingSingleton(using Type[CapturingSingletonType]) extends CapturingType[Const[HEmpty], CapturingSingletonType] with HNonEmptyType[CapturingSingletonType] {
    override def sanitiseCode[R <: Rep: Type](sanitisedCapture: Expr[SanitisedT[Option, HSingleton[String]]], sanitisedInner: => Expr[SanitisedT[Option, Const[HEmpty][R]]])(using Quotes): Expr[SanitisedT[Option, HSingleton[String]]] = {
      sanitisedCapture
    }

    override def flattenFunction[R <: Rep: Type, C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes, RepType[R]): FlattenFunction[CCons[HSingleton[String], C], L, ?] = {
      nodes.flattenFunction(TCons(Type.of[String], types)) match {
        case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HSingleton[String], C], L, a] {
          override def apply(chains: CCons[HSingleton[String], C], leaves: L)(using Quotes): Expr[a] = {
            val capture = '{ ${ chains.head }.value }
            flatten(chains.tail, LCons(capture, leaves))
          }
        }
      }
    }
  }

  private type CapturingAppendType[F[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HAppend[HSingleton[String], F[R]]
  private class CapturingAppend[F[_ <: Rep] <: HNonEmpty: Type](inner: Tidiable[F])(using Type[CapturingAppendType[F]]) extends CapturingType[F, CapturingAppendType[F]] with HNonEmptyType[CapturingAppendType[F]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedCapture: Expr[SanitisedT[Option, HSingleton[String]]], sanitisedInner: => Expr[SanitisedT[Option, F[R]]])(using Quotes): Expr[SanitisedT[Option, HAppend[HSingleton[String], F[R]]]] = {
      '{
        for {
          capture <- $sanitisedCapture
          inner <- $sanitisedInner
        } yield HAppend(capture, inner)
      }
    }

    override def flattenFunction[R <: Rep: Type, C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes, RepType[R]): FlattenFunction[CCons[CapturingAppendType[F][R], C], L, ?] = {
      inner.flattenFunction(nodes, TCons(Type.of[String], types)) match {
        case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CapturingAppendType[F][R], C], L, a] {
          override def apply(chains: CCons[CapturingAppendType[F][R], C], leaves: L)(using Quotes): Expr[a] = {
            '{
              val node = ${ chains.head }
              ${ flatten(CCons('{ node.right }, chains.tail), LCons('{ node.left.value }, leaves)) }
            }
          }
        }
      }
    }
  }
}
