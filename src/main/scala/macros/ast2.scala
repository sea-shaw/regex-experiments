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
      def unapply(emptyType: EmptyType): true = true
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
        case EmptyType() => new Capture(inner)(CaptureSingleton())
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

    case class Cat[A <: HChain, B <: HChain, T <: HChain] private (left: Regex[A], right: Regex[B])(override val nodeType: CatType[A, B, T]) extends Regex[T] {
      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[T] = {
        given Type[T] = nodeType.tpe

        nodeType match {
          case CatEmpty()  => '{ SanitisedT(Some(Sanitised(HEmpty, false))) }
          case CatLeft(_)  => left.sanitiseCode(groups, i)
          case CatRight(_) => right.sanitiseCode(groups, i + left.numCaptures)
          case CatBoth(given Type[A], given Type[B]) => {
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
        given Type[T] = nodeType.tpe

        nodeType match {
          case CatEmpty() => nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[T, C], L, a] {
              override def apply(chains: CCons[T, C], leaves: L)(using Quotes): Expr[a] = {
                flatten(chains.tail, leaves)
              }
            }
          }
          case CatLeft(_) => left.flattenFunction(nodes, types)
          case CatRight(_) => right.flattenFunction(nodes, types)
          case CatBoth(given Type[A], given Type[B]) => left.flattenFunction(NCons(right, nodes), types) match {
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
        case (EmptyType(), EmptyType()) => new Cat(left, right)(CatEmpty())
        case (leftType: NonEmptyType[A], EmptyType()) => {
          given Type[A] = leftType.tpe
          new Cat(left, right)(CatLeft(leftType.tpe))
        }
        case (EmptyType(), rightType: NonEmptyType[B]) => {
          given Type[B] = rightType.tpe
          new Cat(left, right)(CatRight(rightType.tpe))
        }
        case (leftType: NonEmptyType[A], rightType: NonEmptyType[B]) => {
          given Type[A] = leftType.tpe
          given Type[B] = rightType.tpe
          new Cat(left, right)(CatBoth(leftType.tpe, rightType.tpe))
        }
      }
    }

    sealed trait OptType[A <: HChain, B <: HChain] extends NodeType[B]
    case class OptEmpty()(using Type[HEmpty]) extends OptType[HEmpty, HEmpty] with EmptyType
    case class OptSingleton[A <: HChain](inner: Type[A])(using Type[HSingleton[Option[A]]]) extends OptType[A, HSingleton[Option[A]]] with SingletonType[Option[A]]

    case class Opt[A <: HChain, B <: HChain] private (inner: Regex[A])(override val nodeType: OptType[A, B]) extends Regex[B] {
      override val numCaptures: Int = inner.numCaptures

      override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[B] = {
        given Type[B] = nodeType.tpe

        nodeType match {
          case OptEmpty() => '{ SanitisedT(Some(Sanitised(HEmpty, false))) }
          case OptSingleton(given Type[A]) => {
            val sanitisedInner = inner.sanitiseCode(groups, i)
            '{
              val innerCaps = $sanitisedInner
              SanitisedT(Some(innerCaps.value.sequence.map(HSingleton(_))))
            }
          }
        }
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[B, C], L, ?] = {
        given Type[A] = inner.nodeType.tpe
        given Type[B] = nodeType.tpe

        nodeType match {
          case OptEmpty() => nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[B, C], L, a] {
              override def apply(chains: CCons[B, C], leaves: L)(using Quotes): Expr[a] = flatten(chains.tail, leaves)
            }
          }
          case OptSingleton(_) => inner.tidyFunction match {
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

    sealed trait AltType[Left <: HChain, Right <: HChain, C <: HChain] extends NodeType[C]
    case class AltEmpty()(using Type[HEmpty]) extends AltType[HEmpty, HEmpty, HEmpty] with EmptyType
    case class AltEither[A <: HChain, B <: HChain](left: Type[A], right: Type[B])(using Type[HSingleton[Either[A, B]]]) extends AltType[A, B, HSingleton[Either[A, B]]] with SingletonType[Either[A, B]]

    case class Alt[A <: HChain, B <: HChain, T <: HChain] private (left: Regex[A], right: Regex[B])(override val nodeType: AltType[A, B, T]) extends Regex[T] {
      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode(groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[T] = {
        given Type[T] = nodeType.tpe
        given Type[A] = left.nodeType.tpe
        given Type[B] = right.nodeType.tpe

        nodeType match {
          case AltEmpty() => '{ SanitisedT(Some(Sanitised(HEmpty, false))) }
          case AltEither(_, _) => {
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
        given Type[T] = nodeType.tpe
        given Type[A] = left.nodeType.tpe
        given Type[B] = right.nodeType.tpe

        nodeType match {
          case AltEmpty() => nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[T, C], L, a] {
              override def apply(chains: CCons[T, C], leaves: L)(using Quotes): Expr[a] = {
                flatten(chains.tail, leaves)
              }
            }
          }
          case AltEither(_, _) => (left.tidyFunction, right.tidyFunction) match {
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
  }
}
