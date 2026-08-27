package experiments.macros

import cats.Applicative
import cats.syntax.all.*
import experiments.macros.hcollections.hchain2.*
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
    sealed trait HEmptyType extends NodeType[HEmpty]
    sealed trait HNonEmptyType[A <: HNonEmpty] extends NodeType[A]
    sealed trait HSingletonType[A] extends HNonEmptyType[HSingleton[A]]
    sealed trait HAppendType[A <: HNonEmpty, B <: HNonEmpty] extends HNonEmptyType[HAppend[A, B]]

    sealed abstract class Regex[A <: HChain] {
      val nodeType: NodeType[A]
      val numCaptures: Int

      def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[A]

      final def tidyFunction(using Quotes): TidyFunction[A, ?] = {
        flattenFunction(NNil, TNil) match {
          case flatten @ FlattenFunction(given Type[a]) => new TidyFunction[A, a] {
            override def apply(xs: Expr[A])(using Quotes): Expr[a] = {
              flatten(CCons(xs, CNil), LNil)
            }
          }
        }
      }

      private [AST] def flattenFunction[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[A, C], L, ?]
    }

    case class EmptyType()(using Type[HEmpty]) extends HEmptyType
    sealed abstract class Empty protected (override val nodeType: EmptyType) extends Regex[HEmpty] {
      override final def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[HEmpty] = {
        empty
      }

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
        nodes.flattenFunction(types) match {
          case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HEmpty, C], L, a] {
            override def apply(chains: CCons[HEmpty, C], leaves: L)(using Quotes): Expr[a] = {
              flatten(chains.tail, leaves)
            }
          }
        }
      }
    }

    sealed abstract class EmptyLeaf protected (nodeType: EmptyType) extends Empty(nodeType) {
      override final val numCaptures: Int = 0
    }

    case class Dot private ()(nodeType: EmptyType) extends EmptyLeaf(nodeType)
    object Dot {
      def apply()(using Quotes): Dot = {
        new Dot()(EmptyType())
      }
    }

    case class Lit private (c: Int)(nodeType: EmptyType) extends EmptyLeaf(nodeType)
    object Lit {
      def apply(c: Int)(using Quotes): Lit = {
        new Lit(c)(EmptyType())
      }
    }

    sealed trait CaptureType[A <: HChain, B <: HChain] extends NodeType[B]
    case class CaptureSingleton()(using Type[HSingleton[String]]) extends CaptureType[HEmpty, HSingleton[String]] with HNonEmptyType[HSingleton[String]]
    case class CaptureAppend[A <: HNonEmpty]()(using Type[HAppend[HSingleton[String], A]]) extends CaptureType[A, HAppend[HSingleton[String], A]] with HNonEmptyType[HAppend[HSingleton[String], A]]

    case class Capture[A <: HChain, B <: HChain] private (inner: Regex[A])(override val nodeType: CaptureType[A, B]) extends Regex[B] {
      given Type[A] = inner.nodeType.tpe
      given Type[B] = nodeType.tpe

      override val numCaptures: Int = inner.numCaptures + 1

      override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[B] = {
        val sanitisedCapture = '{
          val sanitised = $groups(${ Expr(i) }).map { s =>
            Sanitised(HSingleton(s), true)
          }
          SanitisedT(sanitised)
        }

        nodeType match {
          case CaptureSingleton() => sanitisedCapture
          case CaptureAppend()    => {
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
        nodeType match {
          case CaptureSingleton() => nodes.flattenFunction(TCons(Type.of[String], types)) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[B, C], L, a] {
              override def apply(chains: CCons[B, C], leaves: L)(using Quotes): Expr[a] = {
                val capture = '{ ${ chains.head }.value }
                flatten(chains.tail, LCons(capture, leaves))
              }
            }
          }
          case CaptureAppend() => inner.flattenFunction(nodes, TCons(Type.of[String], types)) match {
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
        case _: HEmptyType => new Capture(inner)(CaptureSingleton())
        case nonEmptyType: HNonEmptyType[a] => {
          given Type[a] = nonEmptyType.tpe
          new Capture(inner)(CaptureAppend())
        }
      }
    }

    sealed trait CatType[A <: HChain, B <: HChain, C <: HChain] extends NodeType[C]
    case class CatEmpty()(using Type[HEmpty]) extends CatType[HEmpty, HEmpty, HEmpty] with HEmptyType
    case class CatLeft[A <: HNonEmpty]()(using Type[A]) extends CatType[A, HEmpty, A] with HNonEmptyType[A]
    case class CatRight[B <: HNonEmpty]()(using Type[B]) extends CatType[HEmpty, B, B] with HNonEmptyType[B]
    case class CatBoth[A <: HNonEmpty, B <: HNonEmpty]()(using Type[HAppend[A, B]]) extends CatType[A, B, HAppend[A, B]] with HAppendType[A, B]

    case class Cat[A <: HChain, B <: HChain, T <: HChain] private (left: Regex[A], right: Regex[B])(override val nodeType: CatType[A, B, T]) extends Regex[T] {
      given Type[A] = left.nodeType.tpe
      given Type[B] = right.nodeType.tpe
      given Type[T] = nodeType.tpe

      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[T] = {
        nodeType match {
          case CatEmpty() => empty
          case CatLeft()  => left.sanitiseCode(groups, i)
          case CatRight() => right.sanitiseCode(groups, i + left.numCaptures)
          case CatBoth()  => {
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

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[T, C], L, ?] = {
        nodeType match {
          case CatEmpty() => nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[T, C], L, a] {
              override def apply(chains: CCons[T, C], leaves: L)(using Quotes): Expr[a] = {
                flatten(chains.tail, leaves)
              }
            }
          }
          case CatLeft()  => left.flattenFunction(nodes, types)
          case CatRight() => right.flattenFunction(nodes, types)
          case CatBoth()  => left.flattenFunction(NCons(right, nodes), types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[T, C], L, a] {
              override def apply(chains: CCons[T, C], leaves: L)(using Quotes): Expr[a] = {
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

    object Cat {
      def apply[A <: HChain, B <: HChain](left: Regex[A], right: Regex[B])(using Quotes): Cat[A, B, ?] = (left.nodeType, right.nodeType) match {
        case (_: HEmptyType, _: HEmptyType) => new Cat(left, right)(CatEmpty())
        case (leftType: HNonEmptyType[a], _: HEmptyType) => {
          given Type[a] = leftType.tpe
          new Cat(left, right)(CatLeft())
        }
        case (_: HEmptyType, rightType: HNonEmptyType[b]) => {
          given Type[b] = rightType.tpe
          new Cat(left, right)(CatRight())
        }
        case (leftType: HNonEmptyType[a], rightType: HNonEmptyType[b]) => {
          given Type[a] = leftType.tpe
          given Type[b] = rightType.tpe
          new Cat(left, right)(CatBoth())
        }
      }
    }

    sealed trait OptType[A <: HChain, B <: HChain] extends NodeType[B]
    case class OptEmpty()(using Type[HEmpty]) extends OptType[HEmpty, HEmpty] with HEmptyType
    case class OptSingleton[A <: HChain]()(using Type[HSingleton[Option[A]]]) extends OptType[A, HSingleton[Option[A]]] with HSingletonType[Option[A]]

    case class Opt[A <: HChain, B <: HChain] private (inner: Regex[A])(override val nodeType: OptType[A, B]) extends Regex[B] {
      given Type[A] = inner.nodeType.tpe
      given Type[B] = nodeType.tpe

      override val numCaptures: Int = inner.numCaptures

      override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[B] = {
        nodeType match {
          case OptEmpty() => '{ SanitisedT(Some(Sanitised(HEmpty, false))) }
          case OptSingleton() => {
            val sanitisedInner = inner.sanitiseCode(groups, i)
            '{
              val innerCaps = $sanitisedInner
              SanitisedT(Some(innerCaps.value.sequence.map(HSingleton(_))))
            }
          }
        }
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[B, C], L, ?] = {
        nodeType match {
          case OptEmpty() => nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[B, C], L, a] {
              override def apply(chains: CCons[B, C], leaves: L)(using Quotes): Expr[a] = flatten(chains.tail, leaves)
            }
          }
          case OptSingleton() => inner.tidyFunction match {
            case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
              case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[B, C], L, b] {
                override def apply(chains: CCons[B, C], leaves: L)(using Quotes): Expr[b] = {
                  val opt = '{
                    ${ chains.head }.value.map { value =>
                      ${ tidy('value) }
                    }
                  }
                  flatten(chains.tail, LCons(opt, leaves))
                }
              }
            }
          }
        }
      }
    }

    object Opt {
      def apply[A <: HChain](inner: Regex[A])(using Quotes): Opt[A, ?] = inner.nodeType match {
        case _: HEmptyType                  => new Opt(inner)(OptEmpty())
        case nonEmptyType: HNonEmptyType[a] => {
          given Type[a] = nonEmptyType.tpe
          new Opt(inner)(OptSingleton())
        }
      }
    }

    sealed trait AltType[Left <: HChain, Right <: HChain, C <: HChain] extends NodeType[C]
    case class AltEmpty()(using Type[HEmpty]) extends AltType[HEmpty, HEmpty, HEmpty] with HEmptyType
    case class AltEither[A <: HChain, B <: HChain]()(using Type[HSingleton[Either[A, B]]]) extends AltType[A, B, HSingleton[Either[A, B]]] with HSingletonType[Either[A, B]]

    case class Alt[A <: HChain, B <: HChain, T <: HChain] private (left: Regex[A], right: Regex[B])(override val nodeType: AltType[A, B, T]) extends Regex[T] {
      given Type[A] = left.nodeType.tpe
      given Type[B] = right.nodeType.tpe
      given Type[T] = nodeType.tpe

      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[T] = {
        nodeType match {
          case AltEmpty() => '{ SanitisedT(Some(Sanitised(HEmpty, false))) }
          case AltEither() => {
            val sanitisedLeft = left.sanitiseCode(groups, i)
            val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
            '{
              val left = $sanitisedLeft.map(_.asLeft[B])
              val right = $sanitisedRight.map(_.asRight[A])
              (left max right).map(HSingleton(_))
            }
          }
        }
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[T, C], L, ?] = {
        nodeType match {
          case AltEmpty() => nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[T, C], L, a] {
              override def apply(chains: CCons[T, C], leaves: L)(using Quotes): Expr[a] = {
                flatten(chains.tail, leaves)
              }
            }
          }
          case AltEither() => (left.tidyFunction, right.tidyFunction) match {
            case (tidyLeft @ TidyFunction(given Type[a]), tidyRight @ TidyFunction(given Type[b])) => nodes.flattenFunction(TCons(Type.of[Either[a, b]], types)) match {
              case flatten @ FlattenFunction(given Type[c]) => new FlattenFunction[CCons[T, C], L, c] {
                override def apply(chains: CCons[T, C], leaves: L)(using Quotes): Expr[c] = {
                  val alt = '{
                    ${ chains.head }.value.bimap(
                      left => ${ tidyLeft('left) },
                      right => ${ tidyRight('right) }
                    )
                  }
                  flatten(chains.tail, LCons(alt, leaves))
                }
              }
            }
          }
        }
      }
    }

    object Alt {
      def apply[A <: HChain, B <: HChain](left: Regex[A], right: Regex[B])(using Quotes): Alt[A, B, ?] = (left.nodeType, right.nodeType) match {
        case (_: HEmptyType, _: HEmptyType) => new Alt(left, right)(AltEmpty())
        case (leftType, rightType) => {
          given Type[A] = leftType.tpe
          given Type[B] = rightType.tpe
          new Alt(left, right)(AltEither())
        }
      }
    }
  }

  private def empty(using Quotes): SanitiseExpr[HEmpty] = {
    '{ Applicative[[A] =>> SanitisedT[Option, A]].pure(HEmpty) }
  }
}
