package experiments.macros

import cats.Applicative
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

  type Const[A] = [_] =>> A

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

    sealed trait NodeType[F[_ <: Rep] <: HChain](using val tpe: Type[F])
    sealed trait HEmptyType extends NodeType[Const[HEmpty]]
    sealed trait HNonEmptyType[F[_ <: Rep] <: HNonEmpty] extends NodeType[F]

    sealed abstract class Regex[F[_ <: Rep] <: HChain] {
      val nodeType: NodeType[F]
      val numCaptures: Int

      def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[F[R]]

      final def tidyFunction[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = {
        flattenFunction(NNil, TNil) match {
          case flatten @ FlattenFunction(given Type[a]) => new TidyFunction[F[R], a] {
            override def apply(xs: Expr[F[R]])(using Quotes): Expr[a] = {
              flatten(CCons(xs, CNil), LNil)
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
        empty
      }

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
        nodes.flattenFunction(types) match {
          case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HEmpty, C], L, a] {
            override def apply(chains: CCons[HEmpty, C], leaves: L)(using Quotes): Expr[a] = {
              flatten(chains.tail, leaves)
            }
          }
        }
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

    sealed abstract class Capturing[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] protected (inner: Regex[F])(override val nodeType: CapturingType[F, G]) extends Regex[G] {
      given Type[F] = inner.nodeType.tpe
      given Type[G] = nodeType.tpe

      override val numCaptures: Int = inner.numCaptures + 1

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = { 
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

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
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

    case class NonCapture[F[_ <: Rep] <: HChain](flagsOn: Set[Char], flagsOff: Set[Char], inner: Regex[F]) extends Regex[F] {
      override val nodeType: NodeType[F] = inner.nodeType
      override val numCaptures: Int = inner.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[F[R]] = {
        inner.sanitiseCode(groups, i)
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
        inner.flattenFunction(nodes, types)
      }
    }

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

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using rep: RepType[R])(using Quotes): FlattenFunction[CCons[H[R], C], L, ?] = {
        nodeType match {
          case CatEmpty() => flattenEmpty(nodes.flattenFunction(types))
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

    sealed trait AltType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] extends NodeType[H]
    case class AltEmpty()(using Type[Const[HEmpty]]) extends AltType[Const[HEmpty], Const[HEmpty], Const[HEmpty]] with HEmptyType
    type AltBothType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] = [R <: Rep] =>> HSingleton[AltRep[F, G, R, InclusiveOr]]
    case class AltBoth[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain]()(using Type[AltBothType[F, G]]) extends AltType[F, G, AltBothType[F, G]] with HNonEmptyType[AltBothType[F, G]]

    case class Alt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(override val nodeType: AltType[F, G, H]) extends Regex[H] {
      given Type[F] = left.nodeType.tpe
      given Type[G] = right.nodeType.tpe
      given Type[H] = nodeType.tpe

      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using rep: RepType[R])(using Quotes): SanitiseExpr[H[R]] = {
        given Type[InclusiveOr] = inclusiveOrType        

        nodeType match {
          case AltEmpty() => empty
          case AltBoth() => {
            val sanitisedLeft = left.sanitiseCode(groups, i)
            val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
            rep match {
              case RepFalse => '{
                val left = $sanitisedLeft.map(_.asLeft[G[R]])
                val right = $sanitisedRight.map(_.asRight[F[R]])
                (left max right).map(HSingleton(_))
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
          case AltEmpty() => flattenEmpty(nodes.flattenFunction(types))
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
        val nodeType: AltType[F, G, ?] = (left.nodeType, right.nodeType) match {
          case (_: HEmptyType, _: HEmptyType) => AltEmpty()
          case (leftType, rightType) => {
            given Type[F] = leftType.tpe
            given Type[G] = rightType.tpe
            given Type[InclusiveOr] = inclusiveOrType
            AltBoth()
          }
        }
        new Alt(left, right)(nodeType)
      }
    }

    sealed trait OptType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G]
    case class OptEmpty()(using Type[Const[HEmpty]]) extends OptType[Const[HEmpty], Const[HEmpty]] with HEmptyType
    type OptSingletonType[F[_ <: Rep] <: HChain] = [R <: Rep] =>> HSingleton[Option[F[R]]]
    case class OptSingleton[F[_ <: Rep] <: HChain]()(using Type[OptSingletonType[F]]) extends OptType[F, OptSingletonType[F]] with HNonEmptyType[OptSingletonType[F]]

    case class Opt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], quantifierType: QuantifierType)(override val nodeType: OptType[F, G]) extends Regex[G] {
      given Type[F] = inner.nodeType.tpe
      given Type[G] = nodeType.tpe

      override val numCaptures: Int = inner.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
        nodeType match {
          case OptEmpty() => empty
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
          case OptEmpty()     => flattenEmpty(nodes.flattenFunction(types))
          case OptSingleton() => inner.tidyFunction match {
            case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
              case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[G[R], C], L, b] {
                override def apply(chains: CCons[G[R], C], leaves: L)(using Quotes): Expr[b] = {
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
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(using Quotes): Opt[F, ?] = {
        val nodeType: OptType[F, ?] = inner.nodeType match {
          case _: HEmptyType => OptEmpty()
          case nonEmptyType: HNonEmptyType[f] => {
            given Type[f] = nonEmptyType.tpe
            OptSingleton()
          }
        }
        new Opt(inner, quantifierType)(nodeType)
      }
    }

    private def flattenEmpty[C <: Chains, L <: Leaves, A](flatten: FlattenFunction[C, L, A]): FlattenFunction[CCons[HEmpty, C], L, A] = {
      given Type[A] = flatten.tpe
      new FlattenFunction[CCons[HEmpty, C], L, A] {
        override def apply(chains: CCons[HEmpty, C], leaves: L)(using Quotes): Expr[A] = flatten(chains.tail, leaves)
      }
    }

    sealed trait PlusType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] extends NodeType[G]
    case class PlusEmpty()(using Type[Const[HEmpty]]) extends PlusType[Const[HEmpty], Const[HEmpty]] with HEmptyType
    type PlusNonEmptyType[F[_ <: Rep] <: HNonEmpty] = Const[F[true]]
    case class PlusNonEmpty[F[_ <: Rep] <: HNonEmpty]()(using Type[PlusNonEmptyType[F]]) extends PlusType[F, PlusNonEmptyType[F]] with HNonEmptyType[PlusNonEmptyType[F]]

    case class Plus[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(override val nodeType: PlusType[F, G]) extends Regex[G] {
      override val numCaptures: Int = inner.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
        nodeType match {
          case PlusEmpty() => empty
          case PlusNonEmpty() => inner.sanitiseCode(groups, i)(using RepTrue)
        }
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
        nodeType match {
          case PlusEmpty() => flattenEmpty(nodes.flattenFunction(types))
          case PlusNonEmpty() => inner.flattenFunction(nodes, types)(using RepTrue)
        }
      }
    }

    object Plus {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(using Quotes): Plus[F, ?] = {
        val nodeType: PlusType[F, ?] = inner.nodeType match {
          case _: HEmptyType => PlusEmpty()
          case nonEmptyType: HNonEmptyType[f] => {
            given Type[f] = nonEmptyType.tpe
            PlusNonEmpty()
          }
        }
        new Plus(inner, quantifierType)(nodeType)
      }
    }
  }

  sealed trait QuantifierType
  case object Greedy extends QuantifierType
  case object Reluctant extends QuantifierType
  case object Possessive extends QuantifierType

  private def empty(using Quotes): SanitiseExpr[HEmpty] = {
    '{ Applicative[[A] =>> SanitisedT[Option, A]].pure(HEmpty) }
  }
}
