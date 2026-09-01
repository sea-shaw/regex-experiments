package experiments.macros

import cats.collections.Diet
import cats.syntax.all.*
import experiments.macros.hcollections.hchain.*
import experiments.macros.sanitised.{SanitiseExpr, Sanitised, SanitisedT}
import scala.quoted.{Expr, Type, Quotes}

object ast {
  type Groups = Array[Option[String]]

  type Rep = Boolean
  sealed trait RepType[R <: Rep]
  case object RepTrue extends RepType[true]
  case object RepFalse extends RepType[false]

  type Id[+A] = A
  type Const[+A] = [_] =>> A

  /* This has to be outside the `AST` trait otherwise there is a compiler error.
     It says it needs a `Type[AST.this.AltRep]`. */
  type AltRep[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, R <: Rep, InclusiveOr[+_ <: HChain, +_ <: HChain]] = R match {
    case false => Either[F[R], G[R]]
    case true  => InclusiveOr[F[R], G[R]]
  }

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

    private case class NCons[F[_ <: Rep] <: HChain, R <: Rep: Type, C <: Chains](regex: Regex[F], rep: RepType[R], tail: Nodes[C]) extends Nodes[CCons[F[R], C]] {
      override private [AST] def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
        given RepType[R] = rep
        regex.flattenFunction(tail, types)
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
      def apply(chain: Expr[A])(using Quotes): Expr[B]
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

    sealed trait NodeType[F[_ <: Rep] <: HChain](using val tpe: Type[F])
    sealed trait HEmptyType extends NodeType[Const[HEmpty]]
    sealed trait HNonEmptyType[F[_ <: Rep] <: HNonEmpty] extends NodeType[F]

    type SingletonOptionType[F[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[Option[F[R]]]
    sealed trait SingletonOption[F[_ <: Rep] <: HNonEmpty](using val innerType: Type[F]) extends HNonEmptyType[SingletonOptionType[F]] {
      def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?]
    }

    sealed abstract class Regex[F[_ <: Rep] <: HChain] {
      val nodeType: NodeType[F]
      val numCaptures: Int

      def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[F[R]]

      final def tidyFunction[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = {
        flattenFunction(NNil, TNil) match {
          case flatten @ FlattenFunction(given Type[a]) => new TidyFunction[F[R], a] {
            override def apply(chain: Expr[F[R]])(using Quotes): Expr[a] = {
              flatten(CCons(chain, CNil), LNil)
            }
          }
        }
      }

      private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?]
    }

    case class EmptyType()(using Type[Const[HEmpty]]) extends HEmptyType
    object EmptyType {
      given Quotes => EmptyType = EmptyType()
    }

    sealed abstract class Empty protected (using override val nodeType: EmptyType) extends Regex[Const[HEmpty]] {
      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
        sanitiseEmpty
      }

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
        flattenEmpty(nodes, types)
      }
    }

    sealed abstract class EmptyLeaf protected (using EmptyType) extends Empty {
      override final val numCaptures: Int = 0
    }

    case class Dot private ()(using EmptyType) extends EmptyLeaf
    object Dot {
      def apply()(using Quotes): Dot = new Dot()
    }

    case class Lit private (c: Int)(using EmptyType) extends EmptyLeaf
    object Lit {
      def apply(c: Int)(using Quotes): Lit = new Lit(c)
    }

    case class Class private (cs: Diet[Int])(using EmptyType) extends EmptyLeaf
    object Class {
      def apply(cs: Diet[Int])(using Quotes): Class = {
        new Class(cs)
      }
    }

    case class LineStart private ()(using EmptyType) extends EmptyLeaf
    object LineStart {
      def apply()(using Quotes): LineStart = {
        new LineStart()
      }
    }

    case class LineEnd private ()(using EmptyType) extends EmptyLeaf
    object LineEnd {
      def apply()(using Quotes): LineEnd = {
        new LineEnd()
      }
    }

    case class Backreference private (group: Int)(using EmptyType) extends EmptyLeaf
    object Backreference {
      def apply(group: Int)(using Quotes): Backreference = {
        new Backreference(group)
      }
    }

    case class Flags private (flagsOn: Set[Char], flagsOff: Set[Char])(using EmptyType) extends EmptyLeaf
    object Flags {
      def apply(flagsOn: Set[Char], flagsOff: Set[Char])(using Quotes): Flags = new Flags(flagsOn, flagsOff)
    }

    sealed abstract class EmptyWithInner[F[_ <: Rep] <: HChain] protected (inner: Regex[F])(using EmptyType) extends Empty {
      override final val numCaptures: Int = inner.numCaptures
    }

    // TODO: Should this be allowed?
    /* {0} or {0,0} */
    case class Zero[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using EmptyType) extends EmptyWithInner[F](inner)
    object Zero {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Zero[F] = {
        new Zero(inner)
      }
    }

    case class NegativeLookahead[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using EmptyType) extends EmptyWithInner[F](inner)
    object NegativeLookahead {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): NegativeLookahead[F] = {
        new NegativeLookahead(inner)
      }
    }

    case class NegativeLookbehind[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using EmptyType) extends EmptyWithInner[F](inner)
    object NegativeLookbehind {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): NegativeLookbehind[F] = {
        new NegativeLookbehind(inner)
      }
    }

    sealed trait CapturingType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G]
    type CapturingSingletonType = Const[HSingleton[String]]
    case class CapturingSingleton()(using Type[CapturingSingletonType]) extends CapturingType[Const[HEmpty], CapturingSingletonType] with HNonEmptyType[CapturingSingletonType]

    type CapturingAppendType[F[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HAppend[HSingleton[String], F[R]]
    case class CapturingAppend[F[_ <: Rep] <: HNonEmpty]()(using Type[CapturingAppendType[F]]) extends CapturingType[F, CapturingAppendType[F]] with HNonEmptyType[CapturingAppendType[F]]

    object CapturingType {
      def apply[F[_ <: Rep] <: HChain](innerType: NodeType[F])(using Quotes): CapturingType[F, ?] = {
        given Type[F] = innerType.tpe
        innerType match {
          case _: HEmptyType       => CapturingSingleton()
          case _: HNonEmptyType[_] => CapturingAppend()
        }
      }
    }

    sealed abstract class Capturing[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] protected (inner: Regex[F])(override final val nodeType: CapturingType[F, G]) extends Regex[G] {
      given Type[F] = inner.nodeType.tpe
      given Type[G] = nodeType.tpe

      override final val numCaptures: Int = inner.numCaptures + 1

      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
        val sanitisedCapture = '{
          val sanitised = $groups(${ Expr(i) }).map { s =>
            Sanitised(HSingleton(s), true)
          }
          SanitisedT(sanitised)
        }

        nodeType match {
          case CapturingSingleton() => sanitisedCapture
          case CapturingAppend()    => {
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

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
        nodeType match {
          case CapturingSingleton() => nodes.flattenFunction(TCons(Type.of[String], types)) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[G[R], C], L, a] {
              override def apply(chains: CCons[G[R], C], leaves: L)(using Quotes): Expr[a] = {
                val capture = '{ ${ chains.head }.value }
                flatten(chains.tail, LCons(capture, leaves))
              }
            }
          }
          case CapturingAppend() => inner.flattenFunction(nodes, TCons(Type.of[String], types)) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[G[R], C], L, a] {
              override def apply(chains: CCons[G[R], C], leaves: L)(using Quotes): Expr[a] = {
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

    case class Capture[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F])(nodeType: CapturingType[F, G]) extends Capturing[F, G](inner)(nodeType)
    object Capture {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Capture[F, ?] = {
        new Capture(inner)(CapturingType(inner.nodeType))
      }
    }

    case class NamedCapture[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (name: String, inner: Regex[F])(nodeType: CapturingType[F, G]) extends Capturing[F, G](inner)(nodeType)
    object NamedCapture {
      def apply[F[_ <: Rep] <: HChain](name: String, inner: Regex[F])(using Quotes): NamedCapture[F, ?] = {
        new NamedCapture(name, inner)(CapturingType(inner.nodeType))
      }
    }

    sealed abstract class Wrapper[F[_ <: Rep] <: HChain] protected (inner: Regex[F]) extends Regex[F] {
      override final val nodeType: NodeType[F] = inner.nodeType

      override final val numCaptures: Int = inner.numCaptures

      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[F[R]] = {
        inner.sanitiseCode(groups, i)
      }

      override final private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
        inner.flattenFunction(nodes, types)
      }
    }

    case class NonCapture[F[_ <: Rep] <: HChain](flagsOn: Set[Char], flagsOff: Set[Char], inner: Regex[F]) extends Wrapper[F](inner)
    case class PositiveLookahead[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)
    case class PositiveLookbehind[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)
    case class Independent[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)

    sealed trait CatType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] extends NodeType[H]
    case class CatEmpty()(using Type[Const[HEmpty]]) extends CatType[Const[HEmpty], Const[HEmpty], Const[HEmpty]] with HEmptyType
    case class CatLeft[F[_ <: Rep] <: HNonEmpty]()(using Type[F]) extends CatType[F, Const[HEmpty], F] with HNonEmptyType[F]
    case class CatRight[G[_ <: Rep] <: HNonEmpty]()(using Type[G]) extends CatType[Const[HEmpty], G, G] with HNonEmptyType[G]
    type CatBothType[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HAppend[F[R], G[R]]
    case class CatBoth[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty]()(using Type[CatBothType[F, G]]) extends CatType[F, G, CatBothType[F, G]] with HNonEmptyType[CatBothType[F, G]]

    case class Cat[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(override val nodeType: CatType[F, G, H]) extends Regex[H] {
      given Type[F] = left.nodeType.tpe
      given Type[G] = right.nodeType.tpe
      given Type[H] = nodeType.tpe

      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[H[R]] = {
        nodeType match {
          case CatEmpty() => sanitiseEmpty
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

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using rep: RepType[R])(using Quotes): FlattenFunction[CCons[H[R], C], L, ?] = {
        nodeType match {
          case CatEmpty() => flattenEmpty(nodes, types)
          case CatLeft()  => left.flattenFunction(nodes, types)
          case CatRight() => right.flattenFunction(nodes, types)
          case CatBoth()  => left.flattenFunction(NCons(right, rep, nodes), types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[H[R], C], L, a] {
              override def apply(chains: CCons[H[R], C], leaves: L)(using Quotes): Expr[a] = {
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
      def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G])(using Quotes): Cat[F, G, ?] = {
        val nodeType: CatType[F, G, ?] = (left.nodeType, right.nodeType) match {
          case (_: HEmptyType, _: HEmptyType) => CatEmpty()
          case (leftType: HNonEmptyType[f], _: HEmptyType) => {
            given Type[f] = leftType.tpe
            CatLeft()
          }
          case (_: HEmptyType, rightType: HNonEmptyType[g]) => {
            given Type[g] = rightType.tpe
            CatRight()
          }
          case (leftType: HNonEmptyType[f], rightType: HNonEmptyType[g]) => {
            given Type[f] = leftType.tpe
            given Type[g] = rightType.tpe
            CatBoth()
          }
        }
        new Cat(left, right)(nodeType)
      }
    }

    type AltSingleton[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[AltRep[F, G, R, InclusiveOr]]
    type AltSingletonOption[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[Option[AltSingleton[F, G][R]]]
    sealed trait AltType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] extends NodeType[H]

    /* A|B */
    case class AltEmpty()(using Type[Const[HEmpty]]) extends AltType[Const[HEmpty], Const[HEmpty], Const[HEmpty]] with HEmptyType

    /* (A)|B */
    type AltLeftType = SingletonOptionType
    case class AltLeft[F[_ <: Rep] <: HNonEmpty]()(left: Regex[F])(using Type[F], Type[AltLeftType[F]]) extends AltType[F, Const[HEmpty], AltLeftType[F]] with SingletonOption[F] {
      override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = left.tidyFunction
    }

    /* A|(B) */
    type AltRightType = SingletonOptionType
    case class AltRight[G[_ <: Rep] <: HNonEmpty]()(right: Regex[G])(using Type[G], Type[AltRightType[G]]) extends AltType[Const[HEmpty], G, AltRightType[G]] with SingletonOption[G] {
      override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[G[R], ?] = right.tidyFunction
    }

    /* (A)?|B */
    type AltLeftOptionType = SingletonOptionType
    case class AltLeftOption[F[_ <: Rep] <: HNonEmpty]()(leftType: SingletonOption[F])(using Type[F], Type[AltLeftOptionType[F]]) extends AltType[AltLeftOptionType[F], Const[HEmpty], AltLeftOptionType[F]] with SingletonOption[F] {
      override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = leftType.tidyInner
    }

    /* A|(B)? */
    type AltRightOptionType = SingletonOptionType
    case class AltRightOption[G[_ <: Rep] <: HNonEmpty]()(rightType: SingletonOption[G])(using Type[G], Type[AltRightOptionType[G]]) extends AltType[Const[HEmpty], AltRightOptionType[G], AltRightOptionType[G]] with SingletonOption[G] {
      override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[G[R], ?] = rightType.tidyInner
    }

    /* (A)?|(B)? */
    type AltBothOptionType = AltSingletonOption
    case class AltBothOption[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty](left: Type[F], right: Type[G])(leftType: SingletonOption[F], rightType: SingletonOption[G])(using Type[AltBothOptionType[F, G]], Type[AltSingleton[F, G]]) extends AltType[SingletonOptionType[F], SingletonOptionType[G], AltBothOptionType[F, G]] with SingletonOption[AltSingleton[F, G]] {
      override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
        given Type[F] = left
        given Type[G] = right
        tidyAlt(leftType.tidyInner, rightType.tidyInner)
      }
    }

    /* (A)?|(B) */
    type AltBothLeftOptionType = AltSingletonOption
    case class AltBothLeftOption[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty](left: Type[F])(leftType: SingletonOption[F], right: Regex[G])(using Type[AltBothLeftOptionType[F, G]], Type[AltSingleton[F, G]]) extends AltType[SingletonOptionType[F], G, AltBothLeftOptionType[F, G]] with SingletonOption[AltSingleton[F, G]] {
      override def tidyInner[R <: Rep: Type](using rep: RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
        given Type[F] = left
        given Type[G] = right.nodeType.tpe
        tidyAlt(leftType.tidyInner, right.tidyFunction)
      }
    }

    /* (A)|(B)? */
    type AltBothRightOptionType = AltSingletonOption
    case class AltBothRightOption[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty](right: Type[G])(left: Regex[F], rightType: SingletonOption[G])(using Type[AltBothRightOptionType[F, G]], Type[AltSingleton[F, G]]) extends AltType[F, SingletonOptionType[G], AltBothRightOptionType[F, G]] with SingletonOption[AltSingleton[F, G]]{
      override def tidyInner[R <: Rep: Type](using rep: RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
        given Type[F] = left.nodeType.tpe
        given Type[G] = right
        tidyAlt(left.tidyFunction, rightType.tidyInner)
      }
    }

    /* (A)|(B) */
    type AltBothType = AltSingleton
    case class AltBoth[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty]()(using Type[AltBothType[F, G]]) extends AltType[F, G, AltBothType[F, G]] with HNonEmptyType[AltBothType[F, G]]

    private def tidyAlt[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type, R <: Rep: Type, A, B](tidyLeft: TidyFunction[F[R], A], tidyRight: TidyFunction[G[R], B])(using rep: RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
      given Type[A] = tidyLeft.tpe
      given Type[B] = tidyRight.tpe
      given Type[InclusiveOr] = inclusiveOrType

      rep match {
        case RepFalse => new TidyFunction[AltSingleton[F, G][R], Either[A, B]] {
          override def apply(chain: Expr[AltSingleton[F, G][R]])(using Quotes): Expr[Either[A, B]] = {
            '{ $chain.value.bimap(left => ${ tidyLeft('left) }, right => ${ tidyRight('right) }) }
          }
        }
        case RepTrue => new TidyFunction[AltSingleton[F, G][R], InclusiveOr[A, B]] {
          override def apply(chain: Expr[AltSingleton[F, G][R]])(using Quotes): Expr[InclusiveOr[A, B]] = {
            bimap(tidyLeft(_), tidyRight(_))('{ $chain.value })
          }
        }
      }
    }

    case class Alt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(override val nodeType: AltType[F, G, H]) extends Regex[H] {
      given Type[F] = left.nodeType.tpe
      given Type[G] = right.nodeType.tpe
      given Type[H] = nodeType.tpe

      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using rep: RepType[R])(using Quotes): SanitiseExpr[H[R]] = {
        given Type[InclusiveOr] = inclusiveOrType

        nodeType match {
          case AltEmpty()       => sanitiseEmpty
          case AltLeft()        => sanitiseOpt(left, groups, i)
          case AltRight()       => sanitiseOpt(right, groups, i + left.numCaptures)
          case AltLeftOption()  => left.sanitiseCode(groups, i)
          case AltRightOption() => right.sanitiseCode(groups, i + left.numCaptures)
          case AltBothOption(given Type[f], given Type[g]) => {
            val sanitisedLeft = left.sanitiseCode(groups, i)
            val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
            rep match {
              case RepFalse => '{
                val left = $sanitisedLeft.map(_.value.map(_.asLeft[g[R]].singleton))
                val right = $sanitisedRight.map(_.value.map(_.asRight[f[R]].singleton))
                (left max right).map(_.singleton)
              }
              case RepTrue  => '{
                val left = $sanitisedLeft.value.sequence.map(_.flatMap(_.value))
                val right = $sanitisedRight.value.sequence.map(_.flatMap(_.value))
                val caps = (left, right).mapN((left, right) => ${ fromOptions('left, 'right) })
                SanitisedT(caps.traverse(_.map(_.singleton.some.singleton)))
              }
            }
          }
          case AltBothLeftOption(given Type[f]) => {
            val sanitisedLeft = left.sanitiseCode(groups, i)
            val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
            rep match {
              case RepFalse => '{
                val left = $sanitisedLeft.map(_.value.map(_.asLeft[G[R]].singleton))
                val right = $sanitisedRight.map(_.asRight[f[R]].singleton.some)
                (left max right).map(_.singleton)
              }
              case RepTrue  => '{
                val left = $sanitisedLeft.value.sequence.map(_.flatMap(_.value))
                val right = $sanitisedRight.value.sequence
                val caps = (left, right).mapN((left, right) => ${ fromOptions('left, 'right) })
                SanitisedT(caps.traverse(_.map(_.singleton.some.singleton)))
              }
            }
          }
          case AltBothRightOption(given Type[g]) => {
            val sanitisedLeft = left.sanitiseCode(groups, i)
            val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
            rep match {
              case RepFalse => '{
                val left = $sanitisedLeft.map(_.asLeft[g[R]].singleton.some)
                val right = $sanitisedRight.map(_.value.map(_.asRight[F[R]].singleton))
                (left max right).map(_.singleton)
              }
              case RepTrue  => '{
                val left = $sanitisedLeft.value.sequence
                val right = $sanitisedRight.value.sequence.map(_.flatMap(_.value))
                val caps = (left, right).mapN((left, right) => ${ fromOptions('left, 'right) })
                SanitisedT(caps.traverse(_.map(_.singleton.some.singleton)))
              }
            }
          }
          case AltBoth()        => {
            val sanitisedLeft = left.sanitiseCode(groups, i)
            val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
            rep match {
              case RepFalse => '{
                val left = $sanitisedLeft.map(_.asLeft[G[R]])
                val right = $sanitisedRight.map(_.asRight[F[R]])
                (left max right).map(_.singleton)
              }
              case RepTrue => '{
                val left = $sanitisedLeft.value.sequence
                val right = $sanitisedRight.value.sequence
                val caps = (left, right).mapN((left, right) => ${ fromOptions('left, 'right) })
                SanitisedT(caps.traverse(_.map(HSingleton(_))))
              }
            }
          }
        }
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using rep: RepType[R])(using Quotes): FlattenFunction[CCons[H[R], C], L, ?] = {
        given Type[InclusiveOr] = inclusiveOrType

        nodeType match {
          case AltEmpty()       => flattenEmpty(nodes, types)
          case AltLeft()        => flattenOpt(left, nodes, types)
          case AltRight()       => flattenOpt(right, nodes, types)
          case AltLeftOption()  => left.flattenFunction(nodes, types)
          case AltRightOption() => right.flattenFunction(nodes, types)
          case altBothOption @ AltBothOption(given Type[f], given Type[g]) => altBothOption.tidyInner match {
            case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
              case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[H[R], C], L, b] {
                override def apply(chains: CCons[H[R], C], leaves: L)(using Quotes): Expr[b] = {
                  val alt = '{ ${ chains.head }.value.map(node => ${ tidy('node) }) }
                  flatten(chains.tail, LCons(alt, leaves))
                }
              }
            }
          }
          case altBothLeftOption @ AltBothLeftOption(given Type[f]) => altBothLeftOption.tidyInner match {
            case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
              case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[H[R], C], L, b] {
                override def apply(chains: CCons[H[R], C], leaves: L)(using Quotes): Expr[b] = {
                  val alt = '{ ${ chains.head }.value.map(node => ${ tidy('node) }) }
                  flatten(chains.tail, LCons(alt, leaves))
                }
              }
            }
          }
          case altBothRightOption @ AltBothRightOption(given Type[g]) => altBothRightOption.tidyInner match {
            case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
              case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[H[R], C], L, b] {
                override def apply(chains: CCons[H[R], C], leaves: L)(using Quotes): Expr[b] = {
                  val alt = '{ ${ chains.head }.value.map(node => ${ tidy('node) }) }
                  flatten(chains.tail, LCons(alt, leaves))
                }
              }
            }
          }
          case AltBoth() => (left.tidyFunction, right.tidyFunction) match {
            case (tidyLeft @ TidyFunction(given Type[a]), tidyRight @ TidyFunction(given Type[b])) => rep match {
              case RepFalse => nodes.flattenFunction(TCons(Type.of[Either[a, b]], types)) match {
                case flatten @ FlattenFunction(given Type[c]) => new FlattenFunction[CCons[H[R], C], L, c] {
                  override def apply(chains: CCons[H[R], C], leaves: L)(using Quotes): Expr[c] = {
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
              case RepTrue => nodes.flattenFunction(TCons(Type.of[InclusiveOr[a, b]], types)) match {
                case flatten @ FlattenFunction(given Type[c]) => new FlattenFunction[CCons[H[R], C], L, c] {
                  override def apply(chains: CCons[H[R], C], leaves: L)(using Quotes): Expr[c] = {
                    val alt = '{
                      val alt = ${ chains.head }.value
                      ${ bimap(tidyLeft(_), tidyRight(_))('alt) }
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

    object Alt {
      def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G])(using Quotes): Alt[F, G, ?] = {
        given Type[InclusiveOr] = inclusiveOrType

        val nodeType: AltType[F, G, ?] = (left.nodeType, right.nodeType) match {
          case (_: HEmptyType, _: HEmptyType) => AltEmpty()
          case (leftType: SingletonOption[f], _: HEmptyType) => {
            given Type[f] = leftType.innerType
            AltLeftOption()(leftType)
          }
          case (_: HEmptyType, rightType: SingletonOption[g]) => {
            given Type[g] = rightType.innerType
            AltRightOption()(rightType)
          }
          case (leftType: HNonEmptyType[f], _: HEmptyType) => {
            given Type[f] = leftType.tpe
            AltLeft()(left)
          }
          case (_: HEmptyType, rightType: HNonEmptyType[g]) => {
            given Type[g] = rightType.tpe
            AltRight()(right)
          }
          case (leftType: SingletonOption[f], rightType: SingletonOption[g]) => {
            given Type[f] = leftType.innerType
            given Type[g] = rightType.innerType
            AltBothOption(leftType.innerType, rightType.innerType)(leftType, rightType)
          }
          case (leftType: SingletonOption[f], rightType: HNonEmptyType[g]) => {
            given Type[f] = leftType.innerType
            given Type[g] = rightType.tpe
            AltBothLeftOption(leftType.innerType)(leftType, right)
          }
          case (leftType: HNonEmptyType[f], rightType: SingletonOption[g]) => {
            given Type[f] = leftType.tpe
            given Type[g] = rightType.innerType
            AltBothRightOption(rightType.innerType)(left, rightType)
          }
          case (leftType: HNonEmptyType[f], rightType: HNonEmptyType[g]) => {
            given Type[f] = leftType.tpe
            given Type[g] = rightType.tpe
            AltBoth()
          }
        }
        new Alt(left, right)(nodeType)
      }
    }

    sealed trait OptType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G]

    /* A? */
    case class OptEmpty()(using Type[Const[HEmpty]]) extends OptType[Const[HEmpty], Const[HEmpty]] with HEmptyType

    /* (A)? */
    type OptSingletonType = SingletonOptionType
    case class OptSingleton[F[_ <: Rep] <: HNonEmpty]()(inner: Regex[F])(using Type[F], Type[OptSingletonType[F]]) extends OptType[F, OptSingletonType[F]] with SingletonOption[F] {
      override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = inner.tidyFunction
    }

    /* (A?)? */
    type OptNestedType = SingletonOptionType
    case class OptNested[F[_ <: Rep] <: HNonEmpty]()(innerType: SingletonOption[F])(using Type[F], Type[OptNestedType[F]]) extends OptType[OptNestedType[F], OptNestedType[F]] with SingletonOption[F] {
      override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = innerType.tidyInner
    }

    case class Opt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], quantifierType: QuantifierType)(override val nodeType: OptType[F, G]) extends Regex[G] {
      given Type[F] = inner.nodeType.tpe
      given Type[G] = nodeType.tpe

      override val numCaptures: Int = inner.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
        nodeType match {
          case OptEmpty() => sanitiseEmpty
          case OptNested() => inner.sanitiseCode(groups, i)
          case OptSingleton() => {
            val sanitisedInner = inner.sanitiseCode(groups, i)
            '{
              val innerCaps = $sanitisedInner
              SanitisedT(Some(innerCaps.value.sequence.map(HSingleton(_))))
            }
          }
        }
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
        nodeType match {
          case OptEmpty()     => flattenEmpty(nodes, types)
          case OptNested()    => inner.flattenFunction(nodes, types)
          case OptSingleton() => flattenOpt(inner, nodes, types)
        }
      }
    }

    object Opt {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(using Quotes): Opt[F, ?] = {
        val nodeType: OptType[F, ?] = inner.nodeType match {
          case _: HEmptyType => OptEmpty()
          case singletonOption: SingletonOption[f] => {
            given Type[f] = singletonOption.innerType
            OptNested()(singletonOption)
          }
          case nonEmpty: HNonEmptyType[f] => {
            given Type[f] = nonEmpty.tpe
            OptSingleton()(inner)
          }
        }
        new Opt(inner, quantifierType)(nodeType)
      }
    }

    sealed trait Rep1Type[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G]
    case class Rep1Empty()(using Type[Const[HEmpty]]) extends Rep1Type[Const[HEmpty], Const[HEmpty]] with HEmptyType
    type Rep1NonEmptyType[F[_ <: Rep] <: HNonEmpty] = Const[F[true]]
    case class Rep1NonEmpty[F[_ <: Rep] <: HNonEmpty]()(using Type[Rep1NonEmptyType[F]]) extends Rep1Type[F, Rep1NonEmptyType[F]] with HNonEmptyType[Rep1NonEmptyType[F]]

    object Rep1Type {
      def apply[F[_ <: Rep] <: HChain](innerType: NodeType[F])(using Quotes): Rep1Type[F, ?] = {
        given Type[F] = innerType.tpe
        innerType match {
          case _: HEmptyType       => Rep1Empty()
          case _: HNonEmptyType[_] => Rep1NonEmpty()
        }
      }
    }

    sealed abstract class Rep1[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F])(override final val nodeType: Rep1Type[F, G]) extends Regex[G] {
      override final val numCaptures: Int = inner.numCaptures

      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
        nodeType match {
          case Rep1Empty() => sanitiseEmpty
          case Rep1NonEmpty() => inner.sanitiseCode(groups, i)(using RepTrue)
        }
      }

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
        nodeType match {
          case Rep1Empty() => flattenEmpty(nodes, types)
          case Rep1NonEmpty() => inner.flattenFunction(nodes, types)(using RepTrue)
        }
      }
    }

    case class Plus[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1(inner)(nodeType)
    object Plus {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(using Quotes): Plus[F, ?] = {
        new Plus(inner, quantifierType)(Rep1Type(inner.nodeType))
      }
    }

    /* {n} for n >= 2. */
    case class Exactly[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
    object Exactly {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType)(using Quotes): Exactly[F, ?] = {
        new Exactly(inner, n, quantifierType)(Rep1Type(inner.nodeType))
      }
    }

    /* {n,} for n >= 1. Use `Star` for {0,} */
    case class AtLeast[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
    object AtLeast {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType)(using Quotes): AtLeast[F, ?] = {
        new AtLeast(inner, n, quantifierType)(Rep1Type(inner.nodeType))
      }
    }

    /* {n, m} for m > n >= 1. */
    case class Between[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, m: Int, quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
    object Between {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, m: Int, quantifierType: QuantifierType)(using Quotes): Between[F, ?] = {
        new Between(inner, n, m, quantifierType)(Rep1Type(inner.nodeType))
      }
    }

    sealed trait Rep0Type[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G]
    case class Rep0Empty()(using Type[Const[HEmpty]]) extends Rep0Type[Const[HEmpty], Const[HEmpty]] with HEmptyType

    // TODO: Rep0Nested

    type Rep0NonEmptyType[F[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[Option[F[true]]]
    case class Rep0NonEmpty[F[_ <: Rep] <: HNonEmpty]()(inner: Regex[F])(using Type[Const[F[true]]], Type[Rep0NonEmptyType[F]]) extends Rep0Type[F, Rep0NonEmptyType[F]] with SingletonOption[Const[F[true]]] {
      override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[true], ?] = inner.tidyFunction(using RepTrue)
    }

    object Rep0Type {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Rep0Type[F, ?] = {
        given Type[F] = inner.nodeType.tpe
        inner.nodeType match {
          case _: HEmptyType       => Rep0Empty()
          case _: HNonEmptyType[_] => Rep0NonEmpty()(inner)
        }
      }
    }

    sealed abstract class Rep0[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F])(override final val nodeType: Rep0Type[F, G]) extends Regex[G] {
      given Type[F] = inner.nodeType.tpe
      given Type[G] = nodeType.tpe

      override final val numCaptures: Int = inner.numCaptures

      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
        nodeType match {
          case Rep0Empty()    => sanitiseEmpty
          case Rep0NonEmpty() => sanitiseOpt(inner, groups, i)(using RepTrue)
        }
      }

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
        nodeType match {
          case Rep0Empty()    => flattenEmpty(nodes, types)
          case Rep0NonEmpty() => flattenOpt(inner, nodes, types)(using RepTrue)
        }
      }
    }

    case class Star[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], quantifierType: QuantifierType)(nodeType: Rep0Type[F, G]) extends Rep0[F, G](inner)(nodeType)
    object Star {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(using Quotes) = {
        new Star(inner, quantifierType)(Rep0Type(inner))
      }
    }

    /* {0, m} for m >= 2. Use `Opt` for {0, 1}. */
    case class AtMost[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: Rep0Type[F, G]) extends Rep0[F, G](inner)(nodeType)
    object AtMost {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType)(using Quotes) = {
        new AtMost(inner, n, quantifierType)(Rep0Type(inner))
      }
    }

    private def sanitiseOpt[F[_ <: Rep] <: HNonEmpty: Type, R <: Rep: Type](regex: Regex[F], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[SingletonOptionType[F][R]] = {
      val sanitised = regex.sanitiseCode(groups, i)
      '{
        val caps = $sanitised
        SanitisedT(Some(caps.value.sequence.map(HSingleton(_))))
      }
    }

    private def flattenOpt[F[_ <: Rep] <: HNonEmpty: Type, C <: Chains, L <: Leaves, R <: Rep: Type](regex: Regex[F], nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[SingletonOptionType[F][R], C], L, ?] = {
      regex.tidyFunction match {
        case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
          case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[SingletonOptionType[F][R], C], L, b] {
            override def apply(chains: CCons[SingletonOptionType[F][R], C], leaves: L)(using Quotes): Expr[b] = {
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

    private def sanitiseEmpty(using Quotes): SanitiseExpr[HEmpty] = {
      '{ SanitisedT(Some(Sanitised(HEmpty, false))) }
    }

    private def flattenEmpty[C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
      nodes.flattenFunction(types) match {
        case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HEmpty, C], L, a] {
          override def apply(chains: CCons[HEmpty, C], leaves: L)(using Quotes): Expr[a] = flatten(chains.tail, leaves)
        }
      }
    }
  }

  sealed trait QuantifierType
  case object Greedy extends QuantifierType
  case object Reluctant extends QuantifierType
  case object Possessive extends QuantifierType

}
