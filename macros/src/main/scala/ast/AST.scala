package experiments.macros.ast

import cats.collections.Diet
import cats.syntax.all.*
import experiments.macros.hcollections.hchain.*
import experiments.macros.sanitised.{SanitiseExpr, Sanitised, SanitisedT}
import scala.quoted.{Expr, Type, Quotes}

/* This has to be outside the `AST` trait otherwise there is a compiler error.
    It says it needs a `Type[AST.this.AltRep]`. */
type AltRep[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, R <: Rep, InclusiveOr[+_ <: HChain, +_ <: HChain]] = R match {
  case false => Either[F[R], G[R]]
  case true  => InclusiveOr[F[R], G[R]]
}

trait AST extends Functions with BuildFunction {
  type InclusiveOr[+_, +_]
  protected def inclusiveOrType(using Quotes): Type[InclusiveOr]
  protected def fromOptions[A: Type, B: Type](using Quotes): Expr[(Option[A], Option[B]) => Option[InclusiveOr[A, B]]]
  protected def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(expr: Expr[InclusiveOr[A, B]])(using Quotes): Expr[InclusiveOr[C, D]]

  sealed trait NodeType[F[_ <: Rep] <: HChain](using val tpe: Type[F])
  sealed trait HEmptyType extends NodeType[Const[HEmpty]]
  sealed trait HNonEmptyType[F[_ <: Rep] <: HNonEmpty] extends NodeType[F]

  type SingletonOptionType[F[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[Option[F[R]]]
  sealed trait SingletonOption[F[_ <: Rep] <: HNonEmpty](using val innerType: Type[F]) extends HNonEmptyType[SingletonOptionType[F]] {
    def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?]
  }

  sealed abstract class Regex[F[_ <: Rep] <: HChain] extends Tidiable[F] {
    val nodeType: NodeType[F]
    val numCaptures: Int

    def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[F[R]]
  }

  class EmptyType(using Type[Const[HEmpty]]) extends HEmptyType
  object EmptyType {
    given Quotes => EmptyType = EmptyType()
  }

  sealed abstract class Empty protected (using override val nodeType: EmptyType) extends Regex[Const[HEmpty]] {
    override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
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

  sealed trait CapturingType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G] {
    def sanitiseCode[R <: Rep: Type](sanitisedCapture: Expr[SanitisedT[Option, HSingleton[String]]], sanitisedInner: => Expr[SanitisedT[Option, F[R]]])(using Quotes): Expr[SanitisedT[Option, G[R]]]
    private [AST] def flattenFunction[R <: Rep: Type, C <: Chains, L <: Leaves](nodes: Nodes[C], types: Types[L])(using Quotes, RepType[R]): FlattenFunction[CCons[G[R], C], L, ?]
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
  private class CapturingAppend[F[_ <: Rep] <: HNonEmpty: Type](inner: Regex[F])(using Type[CapturingAppendType[F]]) extends CapturingType[F, CapturingAppendType[F]] with HNonEmptyType[CapturingAppendType[F]] {
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

  object CapturingType {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): CapturingType[F, ?] = {
      given Type[F] = inner.nodeType.tpe
      inner.nodeType match {
        case _: HEmptyType       => CapturingSingleton()
        case _: HNonEmptyType[_] => CapturingAppend(inner)
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

      nodeType.sanitiseCode(sanitisedCapture, inner.sanitiseCode(groups, i + 1))
    }

    override final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
      nodeType.flattenFunction(nodes, types)
    }
  }

  case class Capture[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F])(nodeType: CapturingType[F, G]) extends Capturing[F, G](inner)(nodeType)
  object Capture {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Capture[F, ?] = {
      new Capture(inner)(CapturingType(inner))
    }
  }

  case class NamedCapture[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (name: String, inner: Regex[F])(nodeType: CapturingType[F, G]) extends Capturing[F, G](inner)(nodeType)
  object NamedCapture {
    def apply[F[_ <: Rep] <: HChain](name: String, inner: Regex[F])(using Quotes): NamedCapture[F, ?] = {
      new NamedCapture(name, inner)(CapturingType(inner))
    }
  }

  sealed abstract class Wrapper[F[_ <: Rep] <: HChain] protected (inner: Regex[F]) extends Regex[F] {
    override final val nodeType: NodeType[F] = inner.nodeType

    override final val numCaptures: Int = inner.numCaptures

    override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[F[R]] = {
      inner.sanitiseCode(groups, i)
    }

    override final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
      inner.flattenFunction(nodes, types)
    }
  }

  case class NonCapture[F[_ <: Rep] <: HChain](flagsOn: Set[Char], flagsOff: Set[Char], inner: Regex[F]) extends Wrapper[F](inner)
  case class PositiveLookahead[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)
  case class PositiveLookbehind[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)
  case class Independent[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)

  sealed trait CatType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] extends NodeType[H] {
    def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[G[R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[H[R]]
    private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[H[R], C], L, ?]
  }

  private class CatEmpty(using Type[Const[HEmpty]]) extends CatType[Const[HEmpty], Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
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

  private class CatLeft[F[_ <: Rep] <: HNonEmpty](left: Regex[F])(using Type[F]) extends CatType[F, Const[HEmpty], F] with HNonEmptyType[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[F[R]] = {
      sanitisedLeft
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
      left.flattenFunction(nodes, types)
    }
  }

  private class CatRight[G[_ <: Rep] <: HNonEmpty](right: Regex[G])(using Type[G]) extends CatType[Const[HEmpty], G, G] with HNonEmptyType[G] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[G[R]], groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[G[R]] = {
      sanitisedRight
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
      right.flattenFunction(nodes, types)
    }
  }

  private type CatBothType[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HAppend[F[R], G[R]]
  private class CatBoth[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type](left: Regex[F], right: Regex[G])(using Type[CatBothType[F, G]]) extends CatType[F, G, CatBothType[F, G]] with HNonEmptyType[CatBothType[F, G]] {
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

  case class Cat[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(override val nodeType: CatType[F, G, H]) extends Regex[H] {
    given Type[F] = left.nodeType.tpe
    given Type[G] = right.nodeType.tpe
    given Type[H] = nodeType.tpe

    override val numCaptures: Int = left.numCaptures + right.numCaptures

    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[H[R]] = {
      lazy val sanitisedLeft = left.sanitiseCode(groups, i)
      lazy val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
      nodeType.sanitiseCode(sanitisedLeft, sanitisedRight, groups, i)
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using rep: RepType[R])(using Quotes): FlattenFunction[CCons[H[R], C], L, ?] = {
      nodeType.flattenFunction(nodes, types)
    }
  }

  object Cat {
    def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G])(using Quotes): Cat[F, G, ?] = {
      val nodeType: CatType[F, G, ?] = (left.nodeType, right.nodeType) match {
        case (_: HEmptyType, _: HEmptyType) => CatEmpty()
        case (leftType: SingletonOption[f], _: HEmptyType) => {
          given Type[f] = leftType.innerType
          CatLeftOption(leftType)
        }
        case (_: HEmptyType, rightType: SingletonOption[f]) => {
          given Type[f] = rightType.innerType
          CatRightOption(rightType)
        }
        case (leftType: HNonEmptyType[f], _: HEmptyType) => {
          given Type[f] = leftType.tpe
          CatLeft(left)
        }
        case (_: HEmptyType, rightType: HNonEmptyType[g]) => {
          given Type[g] = rightType.tpe
          CatRight(right)
        }
        case (leftType: HNonEmptyType[f], rightType: HNonEmptyType[g]) => {
          given Type[f] = leftType.tpe
          given Type[g] = rightType.tpe
          CatBoth(left, right)
        }
      }
      new Cat(left, right)(nodeType)
    }
  }

  private type AltSingleton[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[AltRep[F, G, R, InclusiveOr]]
  private type AltSingletonOption[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[Option[AltSingleton[F, G][R]]]

  sealed trait AltType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] extends NodeType[H] {
    def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[G[R]])(using RepType[R])(using Quotes): SanitiseExpr[H[R]]
    private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[H[R], C], L, ?]
  }

  /* A|B */
  private class AltEmpty(using Type[Const[HEmpty]]) extends AltType[Const[HEmpty], Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]])(using RepType[R])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HEmpty.type, C], L, ?] = {
      flattenEmpty(nodes, types)
    }
  }

  /* (A)|B */
  type AltLeftType = SingletonOptionType
  private class AltLeft[F[_ <: Rep] <: HNonEmpty](left: Regex[F])(using Type[F], Type[AltLeftType[F]]) extends AltType[F, Const[HEmpty], AltLeftType[F]] with SingletonOption[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]])(using RepType[R])(using Quotes): SanitiseExpr[AltLeftType[F][R]] = {
      sanitiseOpt(sanitisedLeft)
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[F[R]]], C], L, ?] = {
      flattenOpt(left.tidyFunction, nodes, types)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = left.tidyFunction
  }

  /* A|(B) */
  private type AltRightType = SingletonOptionType
  private class AltRight[G[_ <: Rep] <: HNonEmpty](right: Regex[G])(using Type[G], Type[AltRightType[G]]) extends AltType[Const[HEmpty], G, AltRightType[G]] with SingletonOption[G] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[G[R]])(using RepType[R])(using Quotes): SanitiseExpr[AltRightType[G][R]] = {
      sanitiseOpt(sanitisedRight)
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[G[R]]], C], L, ?] = {
      flattenOpt(right.tidyFunction, nodes, types)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[G[R], ?] = right.tidyFunction
  }

  /* (A)?|B */
  private type AltLeftOptionType = SingletonOptionType
  private class AltLeftOption[F[_ <: Rep] <: HNonEmpty](leftType: SingletonOption[F])(using Type[F], Type[AltLeftOptionType[F]]) extends AltType[AltLeftOptionType[F], Const[HEmpty], AltLeftOptionType[F]] with SingletonOption[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[AltLeftOptionType[F][R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]])(using RepType[R])(using Quotes): SanitiseExpr[AltLeftOptionType[F][R]] = {
      sanitisedLeft
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[F[R]]], C], L, ?] = {
      flattenOpt(tidyInner, nodes, types)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = leftType.tidyInner
  }

  /* A|(B)? */
  private type AltRightOptionType = SingletonOptionType
  private class AltRightOption[G[_ <: Rep] <: HNonEmpty](rightType: SingletonOption[G])(using Type[G], Type[AltRightOptionType[G]]) extends AltType[Const[HEmpty], AltRightOptionType[G], AltRightOptionType[G]] with SingletonOption[G] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[AltRightOptionType[G][R]])(using RepType[R])(using Quotes): SanitiseExpr[AltRightOptionType[G][R]] = {
      sanitisedRight
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[G[R]]], C], L, ?] = {
      flattenOpt(tidyInner, nodes, types)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[G[R], ?] = rightType.tidyInner
  }

  /* (A)?|(B)? */
  private type AltBothOptionType = AltSingletonOption
  private class AltBothOption[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type](leftType: SingletonOption[F], rightType: SingletonOption[G])(using Type[AltBothOptionType[F, G]], Type[AltSingleton[F, G]]) extends AltType[SingletonOptionType[F], SingletonOptionType[G], AltBothOptionType[F, G]] with SingletonOption[AltSingleton[F, G]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[SingletonOptionType[F][R]], sanitisedRight: => SanitiseExpr[SingletonOptionType[G][R]])(using RepType[R])(using Quotes): SanitiseExpr[AltBothOptionType[F, G][R]] = {
      sanitiseAlt(
        sanitisedLeft,
        sanitisedRight,
        '{ _.value.map(_.asLeft[G[R]].singleton) },
        '{ _.value.map(_.asRight[F[R]].singleton) },
        flattenSanitised,
        flattenSanitised,
      )
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[AltBothOptionType[F, G][R], C], L, ?] = {
      given Type[InclusiveOr] = inclusiveOrType    

      tidyInner match {
        case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
          case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[AltBothLeftOptionType[F, G][R], C], L, b] {
            override def apply(chains: CCons[AltBothLeftOptionType[F, G][R], C], leaves: L)(using Quotes): Expr[b] = {
              val opt = '{
                ${ chains.head }.value.map { node => ${ tidy('node) } }
              }
              flatten(chains.tail, LCons(opt, leaves))
            }
          }
        }
      }
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
      tidyAlt(leftType.tidyInner, rightType.tidyInner)
    }
  }

  /* (A)?|(B) */
  private type AltBothLeftOptionType = AltSingletonOption
  private class AltBothLeftOption[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type](leftType: SingletonOption[F], rightRegex: Regex[G])(using Type[AltBothLeftOptionType[F, G]], Type[AltSingleton[F, G]]) extends AltType[SingletonOptionType[F], G, AltBothLeftOptionType[F, G]] with SingletonOption[AltSingleton[F, G]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[SingletonOptionType[F][R]], sanitisedRight: => SanitiseExpr[G[R]])(using RepType[R])(using Quotes): SanitiseExpr[AltBothLeftOptionType[F, G][R]] = {
      sanitiseAlt(
        sanitisedLeft,
        sanitisedRight,
        '{ _.value.map(_.asLeft[G[R]].singleton) },
        '{ _.asRight[F[R]].singleton.some },
        flattenSanitised,
        identity,
      )
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using rep: RepType[R])(using Quotes): FlattenFunction[CCons[AltBothLeftOptionType[F, G][R], C], L, ?] = {
      given Type[InclusiveOr] = inclusiveOrType        

      tidyInner match {
        case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
          case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[AltBothLeftOptionType[F, G][R], C], L, b] {
            override def apply(chains: CCons[AltBothLeftOptionType[F, G][R], C], leaves: L)(using Quotes): Expr[b] = {
              val opt = '{
                ${ chains.head }.value.map { node => ${ tidy('node) } }
              }
              flatten(chains.tail, LCons(opt, leaves))
            }
          }
        }
      }
    }

    override def tidyInner[R <: Rep: Type](using rep: RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
      tidyAlt(leftType.tidyInner, rightRegex.tidyFunction)
    }
  }

  /* (A)|(B)? */
  private type AltBothRightOptionType = AltSingletonOption
  private class AltBothRightOption[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type](leftRegex: Regex[F], rightType: SingletonOption[G])(using Type[AltBothRightOptionType[F, G]], Type[AltSingleton[F, G]]) extends AltType[F, SingletonOptionType[G], AltBothRightOptionType[F, G]] with SingletonOption[AltSingleton[F, G]]{
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[SingletonOptionType[G][R]])(using RepType[R])(using Quotes): SanitiseExpr[AltBothRightOptionType[F, G][R]] = {
      sanitiseAlt(
        sanitisedLeft,
        sanitisedRight,
        '{ _.asLeft[G[R]].singleton.some },
        '{ _.value.map(_.asRight[F[R]].singleton) },
        identity,
        flattenSanitised,
      )
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[AltBothRightOptionType[F, G][R], C], L, ?] = {
      given Type[InclusiveOr] = inclusiveOrType        

      tidyInner match {
        case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
          case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[AltBothLeftOptionType[F, G][R], C], L, b] {
            override def apply(chains: CCons[AltBothLeftOptionType[F, G][R], C], leaves: L)(using Quotes): Expr[b] = {
              val opt = '{
                ${ chains.head }.value.map { node => ${ tidy('node) } }
              }
              flatten(chains.tail, LCons(opt, leaves))
            }
          }
        }
      }
    }

    override def tidyInner[R <: Rep: Type](using rep: RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
      tidyAlt(leftRegex.tidyFunction, rightType.tidyInner)
    }
  }

  /* (A)|(B) */
  private type AltBothType = AltSingleton
  private class AltBoth[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type](left: Regex[F], right: Regex[G])(using Type[AltBothType[F, G]]) extends AltType[F, G, AltBothType[F, G]] with HNonEmptyType[AltBothType[F, G]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[G[R]])(using rep: RepType[R])(using Quotes): SanitiseExpr[AltBothType[F, G][R]] = {
      given Type[InclusiveOr] = inclusiveOrType        

      rep match {
        case RepFalse => '{
          val left = $sanitisedLeft.map(_.asLeft[G[R]])
          val right = $sanitisedRight.map(_.asRight[F[R]])
          (left max right).map(_.singleton)
        }
        case RepTrue => '{
          val left = $sanitisedLeft.value.sequence
          val right = $sanitisedRight.value.sequence
          val caps = (left, right).mapN($fromOptions)
          SanitisedT(caps.traverse(_.map(_.singleton)))
        }
      }
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[AltRep[F, G, R, InclusiveOr]], C], L, ?] = {
      tidyAlt(left.tidyFunction, right.tidyFunction) match {
        case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[a], types)) match {
          case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[AltBothType[F, G][R], C], L, b] {
            override def apply(chains: CCons[AltBothType[F, G][R], C], leaves: L)(using Quotes): Expr[b] = {
              flatten(chains.tail, LCons(tidy(chains.head), leaves))
            }
          }
        }
      }
    }
  }

  private def sanitiseAlt[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type, H[_ <: Rep] <: HNonEmpty: Type, I[_ <: Rep] <: HNonEmpty: Type, R <: Rep: Type](
    sanitisedLeft: SanitiseExpr[F[R]],
    sanitisedRight: SanitiseExpr[G[R]],
    leftEither: Expr[F[R] => Option[HSingleton[Either[H[R], I[R]]]]],
    rightEither: Expr[G[R] => Option[HSingleton[Either[H[R], I[R]]]]],
    leftIor: Expr[Sanitised[Option[F[R]]]] => Quotes ?=> Expr[Sanitised[Option[H[R]]]],
    rightIor: Expr[Sanitised[Option[G[R]]]] => Quotes ?=> Expr[Sanitised[Option[I[R]]]],
  )(using rep: RepType[R])(using Quotes): SanitiseExpr[AltSingletonOption[H, I][R]] = {
    given Type[InclusiveOr] = inclusiveOrType      

    rep match {
      case RepFalse => '{
        val left = $sanitisedLeft.map($leftEither)
        val right = $sanitisedRight.map($rightEither)
        (left max right).map(_.singleton)
      }
      case RepTrue  => '{
        val left = ${ leftIor('{ $sanitisedLeft.value.sequence }) }
        val right = ${ rightIor('{ $sanitisedRight.value.sequence }) }
        val caps = (left, right).mapN($fromOptions)
        SanitisedT(caps.traverse(_.map(_.singleton.some.singleton)))
      }
    }
  }

  private def flattenSanitised[F[_ <: Rep] <: HNonEmpty: Type, R <: Rep: Type](expr: Expr[Sanitised[Option[HSingleton[Option[F[R]]]]]])(using Quotes): Expr[Sanitised[Option[F[R]]]] = {
    '{ $expr.map(_.flatMap(_.value)) }
  }

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
      lazy val sanitisedLeft = left.sanitiseCode(groups, i)
      lazy val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)

      nodeType.sanitiseCode(sanitisedLeft, sanitisedRight)
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using rep: RepType[R])(using Quotes): FlattenFunction[CCons[H[R], C], L, ?] = {
      nodeType.flattenFunction(nodes, types)
    }
  }

  object Alt {
    def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G])(using Quotes): Alt[F, G, ?] = {
      given Type[InclusiveOr] = inclusiveOrType

      val nodeType: AltType[F, G, ?] = (left.nodeType, right.nodeType) match {
        case (_: HEmptyType, _: HEmptyType) => AltEmpty()
        case (leftType: SingletonOption[f], _: HEmptyType) => {
          given Type[f] = leftType.innerType
          AltLeftOption(leftType)
        }
        case (_: HEmptyType, rightType: SingletonOption[g]) => {
          given Type[g] = rightType.innerType
          AltRightOption(rightType)
        }
        case (leftType: HNonEmptyType[f], _: HEmptyType) => {
          given Type[f] = leftType.tpe
          AltLeft(left)
        }
        case (_: HEmptyType, rightType: HNonEmptyType[g]) => {
          given Type[g] = rightType.tpe
          AltRight(right)
        }
        case (leftType: SingletonOption[f], rightType: SingletonOption[g]) => {
          given Type[f] = leftType.innerType
          given Type[g] = rightType.innerType
          AltBothOption(leftType, rightType)
        }
        case (leftType: SingletonOption[f], rightType: HNonEmptyType[g]) => {
          given Type[f] = leftType.innerType
          given Type[g] = rightType.tpe
          AltBothLeftOption(leftType, right)
        }
        case (leftType: HNonEmptyType[f], rightType: SingletonOption[g]) => {
          given Type[f] = leftType.tpe
          given Type[g] = rightType.innerType
          AltBothRightOption(left, rightType)
        }
        case (leftType: HNonEmptyType[f], rightType: HNonEmptyType[g]) => {
          given Type[f] = leftType.tpe
          given Type[g] = rightType.tpe
          AltBoth(left, right)
        }
      }
      new Alt(left, right)(nodeType)
    }
  }

  sealed trait OptType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G] {
    def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using RepType[R])(using Quotes): SanitiseExpr[G[R]]
    private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?]
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
  private class OptSingleton[F[_ <: Rep] <: HNonEmpty](inner: Regex[F])(using Type[F], Type[OptSingletonType[F]]) extends OptType[F, OptSingletonType[F]] with SingletonOption[F] {
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

  case class Opt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], quantifierType: QuantifierType)(override val nodeType: OptType[F, G]) extends Regex[G] {
    given Type[F] = inner.nodeType.tpe
    given Type[G] = nodeType.tpe

    override val numCaptures: Int = inner.numCaptures

    override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
      lazy val sanitisedInner = inner.sanitiseCode(groups, i)

      nodeType.sanitiseCode(sanitisedInner)
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
      nodeType.flattenFunction(nodes, types)
    }
  }

  object Opt {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(using Quotes): Opt[F, ?] = {
      val nodeType: OptType[F, ?] = inner.nodeType match {
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
      new Opt(inner, quantifierType)(nodeType)
    }
  }

  sealed trait Rep1Type[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G] {
    def sanitiseCode[R <: Rep](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[G[R]]
    private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?]
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
    private class Rep1NonEmpty[F[_ <: Rep] <: HNonEmpty](inner: Regex[F])(using Type[Rep1NonEmptyType[F]]) extends Rep1Type[F, Rep1NonEmptyType[F]] with HNonEmptyType[Rep1NonEmptyType[F]] {
    override def sanitiseCode[R <: Rep](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[Rep1NonEmptyType[F][R]] = {
      sanitisedInner
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[F[true], C], L, ?] = {
      inner.flattenFunction(nodes, types)(using RepTrue)
    }
  }

  object Rep1Type {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Rep1Type[F, ?] = {
      given Type[F] = inner.nodeType.tpe
      inner.nodeType match {
        case _: HEmptyType       => Rep1Empty()
        case _: HNonEmptyType[_] => Rep1NonEmpty(inner)
      }
    }
  }

  sealed abstract class Rep1[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F])(override final val nodeType: Rep1Type[F, G]) extends Regex[G] {
    override final val numCaptures: Int = inner.numCaptures

    override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
      lazy val sanitisedInner = inner.sanitiseCode(groups, i)(using RepTrue)
      nodeType.sanitiseCode(sanitisedInner)
    }

    override final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
      nodeType.flattenFunction(nodes, types)
    }
  }

  case class Plus[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1(inner)(nodeType)
  object Plus {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(using Quotes): Plus[F, ?] = {
      new Plus(inner, quantifierType)(Rep1Type(inner))
    }
  }

  /* {n} for n >= 2. */
  case class Exactly[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
  object Exactly {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType)(using Quotes): Exactly[F, ?] = {
      new Exactly(inner, n, quantifierType)(Rep1Type(inner))
    }
  }

  /* {n,} for n >= 1. Use `Star` for {0,} */
  case class AtLeast[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
  object AtLeast {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType)(using Quotes): AtLeast[F, ?] = {
      new AtLeast(inner, n, quantifierType)(Rep1Type(inner))
    }
  }

  /* {n, m} for m > n >= 1. */
  case class Between[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, m: Int, quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
  object Between {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, m: Int, quantifierType: QuantifierType)(using Quotes): Between[F, ?] = {
      new Between(inner, n, m, quantifierType)(Rep1Type(inner))
    }
  }

  sealed trait Rep0Type[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G] {
    def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[G[R]]
    private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?]
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
  private class Rep0NonEmpty[F[_ <: Rep] <: HNonEmpty: Type](inner: Regex[F])(using Type[Const[F[true]]], Type[Rep0NonEmptyType[F]]) extends Rep0Type[F, Rep0NonEmptyType[F]] with SingletonOption[Const[F[true]]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[Rep0NonEmptyType[F][R]] = {
      sanitiseOpt(sanitisedInner)
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[F[true]]], C], L, ?] = {
      flattenOpt(inner.tidyFunction(using RepTrue), nodes, types)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[true], ?] = inner.tidyFunction(using RepTrue)
  }

  object Rep0Type {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Rep0Type[F, ?] = {
      given Type[F] = inner.nodeType.tpe
      inner.nodeType match {
        case _: HEmptyType => Rep0Empty()
        case option: SingletonOption[f] => {
          given Type[f] = option.innerType
          Rep0Opt(option)
        }
        case _: HNonEmptyType[_] => Rep0NonEmpty(inner)
      }
    }
  }

  sealed abstract class Rep0[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F])(override final val nodeType: Rep0Type[F, G]) extends Regex[G] {
    given Type[F] = inner.nodeType.tpe
    given Type[G] = nodeType.tpe

    override final val numCaptures: Int = inner.numCaptures

    override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
      lazy val sanitisedInner = inner.sanitiseCode(groups, i)(using RepTrue)
      nodeType.sanitiseCode(sanitisedInner)
    }

    override final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
      nodeType.flattenFunction(nodes, types)
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

  private def sanitiseOpt[F[_ <: Rep] <: HNonEmpty: Type, R <: Rep: Type](sanitised: SanitiseExpr[F[R]])(using Quotes): SanitiseExpr[SingletonOptionType[F][R]] = {
    '{
      val caps = $sanitised
      SanitisedT(Some(caps.value.sequence.map(HSingleton(_))))
    }
  }

  private def flattenOpt[F[_ <: Rep] <: HNonEmpty: Type, R <: Rep: Type, A, C <: Chains, L <: Leaves](tidy: TidyFunction[F[R], A], nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[SingletonOptionType[F][R], C], L, ?] = {
    given Type[A] = tidy.tpe

    nodes.flattenFunction(TCons(Type.of[Option[A]], types)) match {
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
