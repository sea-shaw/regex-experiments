package experiments.macros

import cats.syntax.all.*
import experiments.macros.hcollections.hchain.*
import experiments.macros.sanitised.{SanitiseExpr, Sanitised, SanitisedT}
import scala.quoted.{Expr, Type, Quotes}

object ast2 {
  type Groups = Array[Option[String]]

  trait AST {
    type InclusiveOr[+_, +_]
    protected def inclusiveOrType(using Quotes): Type[InclusiveOr]
    protected def fromOptions[A: Type, B: Type](left: Expr[Option[A]], right: Expr[Option[B]])(using Quotes): Expr[Option[InclusiveOr[A, B]]]
    protected def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(expr: Expr[InclusiveOr[A, B]])(using Quotes): Expr[InclusiveOr[C, D]]
    protected def buildFunction[L <: Leaves](types: Types[L])(using Quotes): BuildFunction[L, ?]

    private sealed trait Nodes[C <: Chains] {
      private [AST] def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[C, L, ?]
    }

    private case object NNil extends Nodes[CNil] {
      override private [AST] def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[CNil, L, ?] = {
        buildFunction(types) match {
          case build @ BuildFunction(given Type[a]) => new FlattenFunction[CNil, L, a] {
            override def apply(chains: CNil, leaves: L)(using Quotes): Expr[a] = {
              build(leaves)
            }
          }
        }
      }
    }

    private case class NCons[A <: HChain, C <: Chains](head: Regex[A], tail: Nodes[C]) extends Nodes[CCons[A, C]] {
      override private [AST] def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[CCons[A, C], L, ?] = {
        head.flattenFunction(tail, types)
      }
    }

    private sealed trait Chains
    private type CNil = CNil.type
    private case object CNil extends Chains
    private case class CCons[A <: HChain, C <: Chains](head: Expr[A], tail: C) extends Chains

    protected sealed trait Types[L <: Leaves]
    protected case object TNil extends Types[LNil]
    protected case class TCons[A, L <: Leaves](head: Type[A], tail: Types[L]) extends Types[LCons[A, L]]

    protected sealed trait Leaves
    protected type LNil = LNil.type
    protected case object LNil extends Leaves
    protected case class LCons[A, L <: Leaves](head: Expr[A], tail: L) extends Leaves

    abstract class TidyFunction[A <: HChain, B](using val tpe: Type[B]) {
      def apply(xs: Expr[A])(using Quotes): Expr[B]
    }

    object TidyFunction {
      def unapply[A <: HChain, B](tidyFunction: TidyFunction[A, B]): Tuple1[Type[B]] = Tuple1(tidyFunction.tpe)
    }

    private abstract class FlattenFunction[C <: Chains, L <: Leaves, A](using val tpe: Type[A]) {
      def apply(chains: C, leaves: L)(using Quotes): Expr[A]
    }

    private object FlattenFunction {
      def unapply[C <: Chains, L <: Leaves, A](tidyFunction: FlattenFunction[C, L, A]): Tuple1[Type[A]] = Tuple1(tidyFunction.tpe)
    }

    protected abstract class BuildFunction[L <: Leaves, A](using val tpe: Type[A]) {
      def apply(leaves: L)(using Quotes): Expr[A]
    }

    protected object BuildFunction {
      def unapply[L <: Leaves, A](buildFunction: BuildFunction[L, A]): Tuple1[Type[A]] = Tuple1(buildFunction.tpe)
    }

    sealed trait NodeType[A <: HChain](using val tpe: Type[A])

    sealed trait EmptyType extends NodeType[HEmpty]
    object EmptyType {
      def unapply(emptyType: EmptyType): EmptyTuple = EmptyTuple
    }

    sealed trait NonEmptyType[A <: HChain] extends NodeType[A]
    object NonEmptyType {
      def unapply[A <: HChain](nonEmptyType: NonEmptyType[A]): Tuple1[Type[A]] = Tuple1(nonEmptyType.tpe)
    }

    sealed trait SingletonType[A] extends NonEmptyType[HSingleton[A]]
    sealed trait AppendType[A <: HChain, B <: HChain] extends NonEmptyType[HAppend[A, B]]

    sealed trait Regex[A <: HChain] {
      val nodeType: NodeType[A]
      val numCaptures: Int

      def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[A]

      private [AST] def flattenFunction[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[A, C], L, ?]
    }


    sealed trait CaptureType[A <: HChain, B <: HChain] extends NodeType[B]
    case class CaptureSingleton()(using Type[HSingleton[String]]) extends CaptureType[HEmpty, HSingleton[String]] with NonEmptyType[HSingleton[String]]
    case class CaptureAppend[A <: HChain](inner: Type[A])(using Type[HAppend[HSingleton[String], A]]) extends CaptureType[A, HAppend[HSingleton[String], A]] with NonEmptyType[ HAppend[HSingleton[String], A]]

    case class Capture[A <: HChain, B <: HChain] private (inner: Regex[A])(override val nodeType: CaptureType[A, B]) extends Regex[B] {
      override val numCaptures: Int = inner.numCaptures + 1

      override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[B] = {
        given Type[B] = nodeType.tpe

        val sanitisedCapture = '{
          val sanitised = $groups(${ Expr(i) }).map { s =>
            Sanitised(HSingleton(s), true)
          }
          SanitisedT(sanitised)
        }

        nodeType match {
          case CaptureSingleton()           => sanitisedCapture
          case CaptureAppend(given Type[A]) => {
            val sanitisedInner = inner.sanitiseCode(groups, i + 1)
            '{
              for {
                capture <- $sanitisedCapture
                inner <- $sanitisedInner
              } yield HAppend(capture, inner)
            }
          }
        }
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[B, C], L, ?] = {
        given Type[B] = nodeType.tpe

        nodeType match {
          case CaptureSingleton() => nodes.flattenFunction(TCons(Type.of[String], types)) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[B, C], L, a] {
              override def apply(chains: CCons[B, C], leaves: L)(using Quotes): Expr[a] = {
                val capture = '{ ${ chains.head }.value }
                flatten(chains.tail, LCons(capture, leaves))
              }
            }
          }
          case CaptureAppend(given Type[A]) => inner.flattenFunction(nodes, TCons(Type.of[String], types)) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[B, C], L, a] {
              override def apply(chains: CCons[B, C], leaves: L)(using Quotes): Expr[a] = {
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

    object Capture {
      def apply[A <: HChain](inner: Regex[A])(using Quotes): Capture[A, ?] = inner.nodeType match {
        case _: EmptyType => new Capture(inner)(CaptureSingleton())
        case nonEmptyType: NonEmptyType[A] => {
          given Type[A] = nonEmptyType.tpe
          new Capture(inner)(CaptureAppend(nonEmptyType.tpe))
        }
      }
    }

    sealed trait CatType[A <: HChain, B <: HChain, C <: HChain] extends NodeType[C]
    case class CatEmpty()(using Type[HEmpty]) extends CatType[HEmpty, HEmpty, HEmpty] with EmptyType
    case class CatLeft[A <: HChain](left: Type[A])(using Type[A]) extends CatType[A, HEmpty, A] with NonEmptyType[A]
    case class CatRight[B <: HChain](right: Type[B])(using Type[B]) extends CatType[HEmpty, B, B] with NonEmptyType[B]
    case class CatBoth[A <: HChain, B <: HChain](left: Type[A], right: Type[B])(using Type[HAppend[A, B]]) extends CatType[A, B, HAppend[A, B]] with AppendType[A, B]

    case class Cat[Left <: HChain, Right <: HChain, A <: HChain] private (left: Regex[Left], right: Regex[Right])(override val nodeType: CatType[Left, Right, A]) extends Regex[A] {
      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[A] = {
        given Type[A] = nodeType.tpe

        nodeType match {
          case CatEmpty()  => '{ SanitisedT(Some(Sanitised(HEmpty, false))) }
          case CatLeft(_)  => left.sanitiseCode(groups, i)
          case CatRight(_) => right.sanitiseCode(groups, i + left.numCaptures)
          case CatBoth(given Type[Left], given Type[Right]) => {
            val sanitisedLeft = left.sanitiseCode(groups, i)
            val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
            '{
              for {
                left <- $sanitisedLeft
                right <- $sanitisedRight
              } yield HAppend(left, right)
            }
          }
        }
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[A, C], L, ?] = {
        given Type[A] = nodeType.tpe

        nodeType match {
          case CatEmpty() => nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[A, C], L, a] {
              override def apply(chains: CCons[A, C], leaves: L)(using Quotes): Expr[a] = {
                flatten(chains.tail, leaves)
              }
            }
          }
          case CatLeft(_)  => left.flattenFunction(nodes, types)
          case CatRight(_) => right.flattenFunction(nodes, types)
          case CatBoth(given Type[Left], given Type[Right]) => left.flattenFunction(NCons(right, nodes), types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[A, C], L, a] {
              override def apply(chains: CCons[A, C], leaves: L)(using Quotes): Expr[a] = {
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

    sealed trait OptType[A <: HChain, B <: HChain] extends NodeType[B]
    case class OptEmpty()(using Type[HEmpty]) extends OptType[HEmpty, HEmpty] with EmptyType
    case class OptNested[A <: HChain](inner: Type[A])(using Type[HSingleton[Option[A]]]) extends OptType[HSingleton[Option[A]], HSingleton[Option[A]]] with SingletonType[Option[A]]
    case class OptSingleton[A <: HChain](inner: Type[A])(using Type[HSingleton[Option[A]]]) extends OptType[A, HSingleton[Option[A]]] with SingletonType[Option[A]]

    sealed trait AltType[A <: HChain, B <: HChain, C <: HChain] extends NodeType[C]
    case class AltEmpty()(using Type[HEmpty]) extends AltType[HEmpty, HEmpty, HEmpty] with EmptyType
    case class AltSingletonEither[A <: HChain, B <: HChain](left: Type[A], right: Type[B])(using Type[HSingleton[Either[A, B]]]) extends AltType[A, B, HSingleton[Either[A, B]]] with SingletonType[Either[A, B]]
    case class AltSingletonIor[A <: HChain, B <: HChain](left: Type[A], right: Type[B])(using Type[HSingleton[InclusiveOr[A, B]]]) extends AltType[A, B, HSingleton[InclusiveOr[A, B]]] with SingletonType[InclusiveOr[A, B]]
  }
}
