package experiments.macros.ast

import cats.collections.Diet
import experiments.macros.hcollections.hchain.*
import experiments.macros.sanitised.{SanitiseExpr, Sanitised, SanitisedT}
import scala.quoted.{Expr, Type, Quotes}

trait AST extends Functions, BuildFunction, CapturingTypes, CatTypes, AltTypes, OptTypes, Rep1Types, Rep0Types {
  sealed abstract class Regex[F[_ <: Rep] <: HChain] extends Tidiable[F] {
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

  sealed abstract class Capturing[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] protected (inner: Regex[F])(override final val nodeType: NodeType[G] & CapturingType[F, G]) extends Regex[G] {
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

  case class Capture[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F])(nodeType: NodeType[G] & CapturingType[F, G]) extends Capturing[F, G](inner)(nodeType)
  object Capture {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Capture[F, ?] = {
      new Capture(inner)(CapturingType(inner).asNodeType)
    }
  }

  case class NamedCapture[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (name: String, inner: Regex[F])(nodeType: NodeType[G] & CapturingType[F, G]) extends Capturing[F, G](inner)(nodeType)
  object NamedCapture {
    def apply[F[_ <: Rep] <: HChain](name: String, inner: Regex[F])(using Quotes): NamedCapture[F, ?] = {
      new NamedCapture(name, inner)(CapturingType(inner).asNodeType)
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

  case class Cat[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(override val nodeType: NodeType[H] & CatType[F, G, H]) extends Regex[H] {
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
      new Cat(left, right)(CatType(left, right).asNodeType)
    }
  }

  case class Alt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(override val nodeType: NodeType[H] & AltType[F, G, H]) extends Regex[H] {
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
      new Alt(left, right)(AltType(left, right).asNodeType)
    }
  }

  case class Opt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], quantifierType: QuantifierType)(override val nodeType: NodeType[G] & OptType[F, G]) extends Regex[G] {
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
      new Opt(inner, quantifierType)(OptType(inner).asNodeType)
    }
  }

  sealed abstract class Rep1[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F])(override final val nodeType: NodeType[G] & Rep1Type[F, G]) extends Regex[G] {
    override final val numCaptures: Int = inner.numCaptures

    override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
      lazy val sanitisedInner = inner.sanitiseCode(groups, i)(using RepTrue)
      nodeType.sanitiseCode(sanitisedInner)
    }

    override final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
      nodeType.flattenFunction(nodes, types)
    }
  }

  case class Plus[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(nodeType: NodeType[G] & Rep1Type[F, G]) extends Rep1(inner)(nodeType)
  object Plus {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(using Quotes): Plus[F, ?] = {
      new Plus(inner, quantifierType)(Rep1Type(inner).asNodeType)
    }
  }

  /* {n} for n >= 2. */
  case class Exactly[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: NodeType[G] & Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
  object Exactly {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType)(using Quotes): Exactly[F, ?] = {
      new Exactly(inner, n, quantifierType)(Rep1Type(inner).asNodeType)
    }
  }

  /* {n,} for n >= 1. Use `Star` for {0,} */
  case class AtLeast[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: NodeType[G] & Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
  object AtLeast {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType)(using Quotes): AtLeast[F, ?] = {
      new AtLeast(inner, n, quantifierType)(Rep1Type(inner).asNodeType)
    }
  }

  /* {n, m} for m > n >= 1. */
  case class Between[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, m: Int, quantifierType: QuantifierType)(nodeType: NodeType[G] & Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
  object Between {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, m: Int, quantifierType: QuantifierType)(using Quotes): Between[F, ?] = {
      new Between(inner, n, m, quantifierType)(Rep1Type(inner).asNodeType)
    }
  }

  sealed abstract class Rep0[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F])(override final val nodeType: NodeType[G] & Rep0Type[F, G]) extends Regex[G] {
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

  case class Star[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], quantifierType: QuantifierType)(nodeType: NodeType[G] & Rep0Type[F, G]) extends Rep0[F, G](inner)(nodeType)
  object Star {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(using Quotes) = {
      new Star(inner, quantifierType)(Rep0Type(inner).asNodeType)
    }
  }

  /* {0, m} for m >= 2. Use `Opt` for {0, 1}. */
  case class AtMost[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: NodeType[G] & Rep0Type[F, G]) extends Rep0[F, G](inner)(nodeType)
  object AtMost {
    def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType)(using Quotes) = {
      new AtMost(inner, n, quantifierType)(Rep0Type(inner).asNodeType)
    }
  }
}
