package experiments.macros

import cats.{Applicative, Eval, Functor, Monad, Traverse}
import cats.collections.Diet
import cats.kernel.Order
import cats.syntax.all.*
import experiments.macros.evidence.{apply, liftCo}
import experiments.macros.hcollections.hchain.{HAppend, HChain, HConcat, HCons, HEmpty, HSingleton}
import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

object ast {

  type Rep = Boolean

  type Const[A] = [_] =>> A

  type Groups = Array[Option[String]]

  case class Sanitised[+A](captures: A, any: Boolean)

  object Sanitised {
    given Functor[Sanitised] {
      override def map[A, B](fa: Sanitised[A])(f: A => B): Sanitised[B] = {
        Sanitised(f(fa.captures), fa.any)
      }
    }

    given Applicative[Sanitised] {
      override def pure[A](x: A): Sanitised[A] = {
        Sanitised(x, false)
      }

      override def ap[A, B](ff: Sanitised[A => B])(fa: Sanitised[A]): Sanitised[B] = {
        Sanitised(ff.captures(fa.captures), ff.any || fa.any)
      }
    }

    given Monad[Sanitised] {
      override def pure[A](x: A): Sanitised[A] = {
        Sanitised(x, false)
      }

      override def flatMap[A, B](fa: Sanitised[A])(f: A => Sanitised[B]): Sanitised[B] = {
        val fb = f(fa.captures)
        Sanitised(fb.captures, fa.any || fb.any)
      }

      override def tailRecM[A, B](a: A)(f: A => Sanitised[Either[A, B]]): Sanitised[B] = {
        @tailrec
        def go(a: A, any: Boolean): Sanitised[B] = f(a) match {
          case Sanitised(Left(value), leftAny) => go(value, any || leftAny)
          case Sanitised(Right(value), rightAny) => Sanitised(value, any || rightAny)
        }

        go(a, false)
      }
    }

    given Traverse[Sanitised] {
      override def foldLeft[A, B](fa: Sanitised[A], b: B)(f: (B, A) => B): B = {
        f(b, fa.captures)
      }

      override def foldRight[A, B](fa: Sanitised[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        f(fa.captures, lb)
      }

      override def traverse[G[_]: Applicative, A, B](fa: Sanitised[A])(f: A => G[B]): G[Sanitised[B]] = {
        f(fa.captures).map(Sanitised(_, fa.any))
      }
    }

    given [A] => Order[Sanitised[A]] = Order.by(_.any)
  }

  case class SanitisedT[F[_], A](value: F[Sanitised[A]])

  object SanitisedT {
    given [F[_]: Functor] => Functor[[A] =>> SanitisedT[F, A]] {
      override def map[A, B](fa: SanitisedT[F, A])(f: A => B): SanitisedT[F, B] = {
        SanitisedT(fa.value.map(_.map(f)))
      }
    }

    given [F[_]: Applicative] => Applicative[[A] =>> SanitisedT[F, A]] {
      override def pure[A](x: A): SanitisedT[F, A] = {
        SanitisedT(Applicative[F].pure(Applicative[Sanitised].pure(x)))
      }

      override def ap[A, B](ff: SanitisedT[F, A => B])(fa: SanitisedT[F, A]): SanitisedT[F, B] = {
        SanitisedT((ff.value, fa.value).mapN(_ ap _))
      }
    }

    given [F[_]: Monad] => Monad[[A] =>> SanitisedT[F, A]] {
      override def pure[A](x: A): SanitisedT[F, A] = {
        SanitisedT(Monad[F].pure(Monad[Sanitised].pure(x)))
      }

      override def flatMap[A, B](fa: SanitisedT[F, A])(f: A => SanitisedT[F, B]): SanitisedT[F, B] = {
        val fb = Monad[F].flatMap(fa.value) { a =>
          f(a.captures).value.map(b => Sanitised(b.captures, a.any || b.any))
        }
        SanitisedT(fb)
      }

      override def tailRecM[A, B](a: A)(f: A => SanitisedT[F, Either[A, B]]): SanitisedT[F, B] = {
        SanitisedT(
          Monad[F].tailRecM((a, false)) { (x, any) =>
            f(x).value.map {
              case Sanitised(Left(left), leftAny) => Left(left, any || leftAny)
              case Sanitised(Right(right), rightAny) => Right(Sanitised(right, any || rightAny))
            }
          }
        )
      }
    }

    given [F[_], A] => Order[F[Sanitised[A]]] => Order[SanitisedT[F, A]] = {
      Order.by(_.value)
    }
  }

  type SanitiseExpr[A] = Expr[SanitisedT[Option, A]]

  // For some reason these can't be inside the trait
  // It says it needs a `Type[AST.this.OptCapture]`
  type OptCapture[A <: HChain] <: HChain = A match {
    case HEmpty => HEmpty
    case _      => HSingleton[Option[A]]
  }

  type AltCapture[A <: HChain, B <: HChain, R <: Rep, InclusiveOr[+_, +_]] <: HChain = (A, B) match {
    case (HEmpty, HEmpty) => HEmpty
    case _                => R match {
      case true  => SingletonWith[InclusiveOr, A, B]
      case false => SingletonWith[Either, A, B]
    }
  }

  type SingletonWith[F[_, _], A <: HChain, B <: HChain] = HSingleton[F[A, B]]

  private def empty(using Quotes): SanitiseExpr[HEmpty] = {
    '{ Applicative[[A] =>> SanitisedT[Option, A]].pure(HEmpty) }
  }

  private def liftSanitised[A <: HChain: Type, B <: HChain: Type](ev: Expr[A =:= B])(using Quotes): Expr[SanitisedT[Option, A] =:= SanitisedT[Option, B]] = {
    ev.liftCo[[X <: HChain] =>> SanitisedT[Option, X]]
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

    private case class NCons[F[_ <: Rep] <: HChain, R <: Rep: Type, C <: Chains](head: Regex[F], tail: Nodes[C]) extends Nodes[CCons[F[R], C]] {
      override private [AST] def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
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

    sealed trait Regex[F[_ <: Rep] <: HChain] {
      val tpe: Type[F]

      val numCaptures: Int

      def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[F[R]]

      final def tidyFunction[R <: Rep: Type](using Quotes): TidyFunction[F[R], ?] = {
        flattenFunction(NNil, TNil) match {
          case flatten @ FlattenFunction(given Type[a]) => new TidyFunction[F[R], a] {
            override def apply(xs: Expr[F[R]])(using Quotes): Expr[a] = {
              flatten(CCons(xs, CNil), LNil)
            }
          }
        }
      }

      private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?]
    }

    type EmptyType = Const[HEmpty]
    sealed abstract class Empty protected (using override val tpe: Type[EmptyType]) extends Regex[EmptyType] {
      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[EmptyType[R]] = {
        empty
      }

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
        nodes.flattenFunction(types) match {
          case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HEmpty, C], L, a] {
            override def apply(chains: CCons[HEmpty, C], leaves: L)(using Quotes): Expr[a] = {
              flatten(chains.tail, leaves)
            }
          }
        }
      }
    }

    sealed abstract class EmptyLeaf protected ()(using Type[EmptyType]) extends Empty {
      override final val numCaptures: Int = 0
    }

    case class Dot private ()(using Type[EmptyType]) extends EmptyLeaf
    object Dot {
      def apply()(using Quotes): Dot = {
        new Dot()
      }
    }

    case class Lit private (c: Int)(using Type[EmptyType]) extends EmptyLeaf
    object Lit {
      def apply(c: Int)(using Quotes): Lit = {
        new Lit(c)
      }
    }

    case class Class private (cs: Diet[Int])(using Type[EmptyType]) extends EmptyLeaf
    object Class {
      def apply(cs: Diet[Int])(using Quotes): Class = {
        new Class(cs)
      }
    }

    case class LineStart private ()(using Type[EmptyType]) extends EmptyLeaf
    object LineStart {
      def apply()(using Quotes): LineStart = {
        new LineStart()
      }
    }

    case class LineEnd private ()(using Type[EmptyType]) extends EmptyLeaf
    object LineEnd {
      def apply()(using Quotes): LineEnd = {
        new LineEnd()
      }
    }

    case class Flags private (on: Set[Char], off: Set[Char])(using Type[EmptyType]) extends EmptyLeaf
    object Flags {
      def apply(on: Set[Char], off: Set[Char])(using Quotes): Flags = {
        new Flags(on, off)
      }
    }

    case class Backreference private (group: Int)(using Type[EmptyType]) extends EmptyLeaf
    object Backreference {
      def apply(group: Int)(using Quotes): Backreference = {
        new Backreference(group)
      }
    }

    sealed abstract class EmptyWithInner[F[_ <: Rep] <: HChain] protected (inner: Regex[F])(using Type[EmptyType]) extends Empty {
      override final val numCaptures: Int = inner.numCaptures      
    }

    // TODO: Should this be allowed?
    /* {0} or {0,0} */
    case class Zero[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using Type[EmptyType]) extends EmptyWithInner[F](inner)
    object Zero {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Zero[F] = {
        new Zero(inner)
      }
    }

    case class NegativeLookahead[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using Type[EmptyType]) extends EmptyWithInner[F](inner)
    object NegativeLookahead {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): NegativeLookahead[F] = {
        new NegativeLookahead(inner)
      }
    }

    case class NegativeLookbehind[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using Type[EmptyType]) extends EmptyWithInner[F](inner)
    object NegativeLookbehind {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): NegativeLookbehind[F] = {
        new NegativeLookbehind(inner)
      }
    }

    type CapturingType[F[_ <: Rep] <: HChain] = [R <: Rep] =>> HCons[String, F[R]]
    sealed abstract class Capturing[F[_ <: Rep] <: HChain] protected (inner: Regex[F])(using override final val tpe: Type[CapturingType[F]]) extends Regex[CapturingType[F]] {
      override final val numCaptures: Int = inner.numCaptures + 1

      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[CapturingType[F][R]] = {
        given Type[F] = inner.tpe

        val sanitisedCapture = '{
          val sanitised = $groups(${ Expr(i) }).map { s =>
            Sanitised(HSingleton(s), true)
          }
          SanitisedT(sanitised)
        }

        val sanitised = Expr.summon[HSingleton[String] =:= CapturingType[F][R]].map { ev =>
          liftSanitised(ev)(sanitisedCapture)
        } orElse Expr.summon[HAppend[HSingleton[String], F[R]] =:= CapturingType[F][R]].map { ev =>
          val sanitisedInner = inner.sanitiseCode(groups, i + 1)
          liftSanitised(ev) {
            '{
              for {
                capture <- $sanitisedCapture
                inner <- $sanitisedInner
              } yield HAppend(capture, inner)
            }
          }
        }
        sanitised.get
      }

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[CapturingType[F][R], C], L, ?] = {
        given Type[F] = inner.tpe

        val flatten = Expr.summon[CapturingType[F][R] =:= HSingleton[String]].map { ev =>
          nodes.flattenFunction(TCons(Type.of[String], types)) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CapturingType[F][R], C], L, a] {
              override def apply(chains: CCons[CapturingType[F][R], C], leaves: L)(using Quotes): Expr[a] = {
                val capture = '{ ${ ev(chains.head) }.value }
                flatten(chains.tail, LCons(capture, leaves))
              }
            }
          }
        } orElse Expr.summon[CapturingType[F][R] =:= HAppend[HSingleton[String], F[R]]].map { ev =>
          inner.flattenFunction(nodes, TCons(Type.of[String], types)) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CapturingType[F][R], C], L, a] {
              override def apply(chains: CCons[CapturingType[F][R], C], leaves: L)(using Quotes): Expr[a] = {
                '{
                  val node = ${ ev(chains.head) }
                  ${ flatten(CCons('{ node.right }, chains.tail), LCons('{ node.left.value }, leaves)) }
                }
              }
            }
          }
        }

        flatten.get
      }
    }

    case class Capture[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using Type[CapturingType[F]]) extends Capturing[F](inner)
    object Capture {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Capture[F] = {
        given Type[F] = inner.tpe
        new Capture(inner)
      }
    }

    case class NamedCapture[F[_ <: Rep] <: HChain] private (name: String, inner: Regex[F])(using Type[CapturingType[F]]) extends Capturing[F](inner)
    object NamedCapture {
      def apply[F[_ <: Rep] <: HChain](name: String, inner: Regex[F])(using Quotes): NamedCapture[F] = {
        given Type[F] = inner.tpe
        new NamedCapture(name, inner)
      }
    }

    sealed abstract class Wrapper[F[_ <: Rep] <: HChain] protected (inner: Regex[F]) extends Regex[F] {
      override final val tpe: Type[F] = inner.tpe

      override final val numCaptures: Int = inner.numCaptures

      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[F[R]] = {
        inner.sanitiseCode(groups, i)
      }

      override final private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
        inner.flattenFunction(nodes, types)
      }
    }

    case class NonCapture[F[_ <: Rep] <: HChain](flagsOn: Set[Char], flagsOff: Set[Char], inner: Regex[F]) extends Wrapper[F](inner) // TOOD: Type of flags
    case class PositiveLookahead[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)
    case class PositiveLookbehind[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)
    case class Independent[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)

    type CatType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] = [R <: Rep] =>> HConcat[F[R], G[R]]
    case class Cat[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(using override val tpe: Type[CatType[F, G]]) extends Regex[CatType[F, G]] {
      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[CatType[F, G][R]] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe

        val sanitised = Expr.summon[F[R] =:= CatType[F, G][R]].map { ev =>
          liftSanitised(ev)(left.sanitiseCode(groups, i))
        } orElse Expr.summon[G[R] =:= CatType[F, G][R]].map { ev =>
          liftSanitised(ev)(right.sanitiseCode(groups, i + left.numCaptures))
        } orElse Expr.summon[HAppend[F[R], G[R]] =:= CatType[F, G][R]].map { ev =>
          val sanitisedLeft = left.sanitiseCode(groups, i)
          val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
          liftSanitised(ev) {
            '{
              for {
                left <- $sanitisedLeft
                right <- $sanitisedRight
              } yield HAppend(left, right)
            }
          }
        }

        sanitised.get
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[CatType[F, G][R], C], L, ?] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe

        val flatten = Expr.summon[CatType[F, G][R] =:= F[R]].map { ev =>
          left.flattenFunction(nodes, types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CatType[F, G][R], C], L, a] {
              override def apply(chains: CCons[CatType[F, G][R], C], leaves: L)(using Quotes): Expr[a] = {
                flatten(CCons(ev(chains.head), chains.tail), leaves)
              }
            }
          }
        } orElse Expr.summon[CatType[F, G][R] =:= G[R]].map { ev =>
          right.flattenFunction(nodes, types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CatType[F, G][R], C], L, a] {
              override def apply(chains: CCons[CatType[F, G][R], C], leaves: L)(using Quotes): Expr[a] = {
                flatten(CCons(ev(chains.head), chains.tail), leaves)
              }
            }
          }
        } orElse Expr.summon[CatType[F, G][R] =:= HAppend[F[R], G[R]]].map { ev =>
          left.flattenFunction(NCons(right, nodes), types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CatType[F, G][R], C], L, a] {
              override def apply(chains: CCons[CatType[F, G][R], C], leaves: L)(using Quotes): Expr[a] = {
                '{
                  val node = ${ ev(chains.head) }
                  ${ flatten(CCons('{ node.left }, CCons('{ node.right }, chains.tail)), leaves) }
                }
              }
            }
          }
        }

        flatten.get
      }
    }

    object Cat {
      def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G])(using Quotes): Cat[F, G] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe
        new Cat(left, right)
      }
    }

    type AltType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] = [R <: Rep] =>> AltCapture[F[R], G[R], R, InclusiveOr]
    case class Alt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(using override val tpe: Type[AltType[F, G]]) extends Regex[AltType[F, G]] {
      override val numCaptures: Int = left.numCaptures + right.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[AltType[F, G][R]] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe
        given Type[InclusiveOr] = inclusiveOrType

        val sanitised = Expr.summon[HEmpty =:= AltType[F, G][R]].map { ev =>
          liftSanitised(ev)(empty)
        } orElse Expr.summon[SingletonWith[Either, F[R], G[R]] =:= AltType[F, G][R]].map { ev =>
          val sanitisedLeft = left.sanitiseCode(groups, i)
          val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
          liftSanitised(ev) {
            '{
              val left = $sanitisedLeft.map(_.asLeft[G[R]])
              val right = $sanitisedRight.map(_.asRight[F[R]])
              (left max right).map(HSingleton(_))
            }
          }
        } orElse Expr.summon[SingletonWith[InclusiveOr, F[R], G[R]] =:= AltType[F, G][R]].map { ev =>
          val sanitisedLeft = left.sanitiseCode(groups, i)
          val sanitisedRight = right.sanitiseCode(groups, i + left.numCaptures)
          liftSanitised(ev) {
            '{
              val left = $sanitisedLeft.value.sequence
              val right = $sanitisedRight.value.sequence
              val caps = (left, right).mapN((l, r) => ${ fromOptions('l, 'r) })
              SanitisedT(caps.traverse(_.map(HSingleton(_))))
            }
          }
        }

        sanitised.get
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[AltType[F, G][R], C], L, ?] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe
        given Type[InclusiveOr] = inclusiveOrType

        val flatten = Expr.summon[AltType[F, G][R] =:= HEmpty].map { _ =>
          nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[AltType[F, G][R], C], L, a] {
              override def apply(chains: CCons[AltType[F, G][R], C], leaves: L)(using Quotes): Expr[a] = {
                flatten(chains.tail, leaves)
              }
            }
          }
        } orElse Expr.summon[AltType[F, G][R] =:= SingletonWith[Either, F[R], G[R]]].map { ev =>
          (left.tidyFunction[R], right.tidyFunction[R]) match {
            case (tidyLeft @ TidyFunction(given Type[a]), tidyRight @ TidyFunction(given Type[b])) => nodes.flattenFunction(TCons(Type.of[Either[a, b]], types)) match {
              case flatten @ FlattenFunction(given Type[c]) => new FlattenFunction[CCons[AltType[F, G][R], C], L, c] {
                override def apply(chains: CCons[AltType[F, G][R], C], leaves: L)(using Quotes): Expr[c] = {
                  val alt = '{
                    ${ ev(chains.head) }.value.bimap(
                      left => ${ tidyLeft('left) },
                      right => ${ tidyRight('right) }
                    )
                  }
                  flatten(chains.tail, LCons(alt, leaves))
                }
              }
            }
          }
        } orElse Expr.summon[AltType[F, G][R] =:= SingletonWith[InclusiveOr, F[R], G[R]]].map { ev =>
          (left.tidyFunction[R], right.tidyFunction[R]) match {
            case (tidyLeft @ TidyFunction(given Type[a]), tidyRight @ TidyFunction(given Type[b])) => nodes.flattenFunction(TCons(Type.of[InclusiveOr[a, b]], types)) match {
              case flatten @ FlattenFunction(given Type[c]) => new FlattenFunction[CCons[AltType[F, G][R], C], L, c] {
                override def apply(chains: CCons[AltType[F, G][R], C], leaves: L)(using Quotes): Expr[c] = {
                  summon[Type[F[R]]]
                  val alt = '{
                    val alt = ${ ev(chains.head) }.value
                    ${ bimap(tidyLeft(_), tidyRight(_))('alt) }
                  }
                  flatten(chains.tail, LCons(alt, leaves))
                }
              }
            }
          }
        }

        flatten.get
      }
    }

    object Alt {
      def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G])(using Quotes): Alt[F, G] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe
        given Type[InclusiveOr] = inclusiveOrType
        new Alt(left, right)
      }
    }

    type OptType[F[_ <: Rep] <: HChain] = [R <: Rep] =>> OptCapture[F[R]] 
    case class Opt[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using override val tpe: Type[OptType[F]]) extends Regex[OptType[F]] {
      override val numCaptures: Int = inner.numCaptures

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[OptType[F][R]] = {
        given Type[F] = inner.tpe

        val sanitised = Expr.summon[HEmpty =:= OptType[F][R]].map { ev =>
          liftSanitised(ev)(empty)
        } orElse Expr.summon[HSingleton[Option[F[R]]] =:= OptType[F][R]].map { ev =>
          val sanitisedInner = inner.sanitiseCode(groups, i)
          liftSanitised(ev) {
            '{
              val innerCaps = $sanitisedInner
              SanitisedT(Some(innerCaps.value.sequence.map(HSingleton(_))))
            }
          }
        }
        sanitised.get
      }

      override private [AST] def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[OptCapture[F[R]], C], L, ?] = {
        given Type[F] = inner.tpe
        val flatten = Expr.summon[OptType[F][R] =:= HEmpty].map { _ =>
          nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[OptCapture[F[R]], C], L, a] {
              override def apply(chains: CCons[OptCapture[F[R]], C], leaves: L)(using Quotes): Expr[a] = {
                flatten(chains.tail, leaves)
              }
            }
          }
        } orElse Expr.summon[OptType[F][R] =:= HSingleton[Option[F[R]]]].map { ev =>
          inner.tidyFunction[R] match {
            case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
              case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[OptCapture[F[R]], C], L, b] {
                override def apply(chains: CCons[OptCapture[F[R]], C], leaves: L)(using Quotes): Expr[b] = {
                  val opt = '{
                    ${ ev(chains.head) }.value.map { value =>
                      ${ tidy('value) }
                    }
                  }
                  flatten(chains.tail, LCons(opt, leaves))
                }
              }
            }
          }
        }

        flatten.get
      }
    }

    object Opt {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Opt[F] = {
        given Type[F] = inner.tpe
        new Opt(inner)
      }
    }

    type Rep1Type[F[_ <: Rep] <: HChain] = Const[F[true]]
    sealed abstract class Rep1[F[_ <: Rep] <: HChain] protected (inner: Regex[F])(using override val tpe: Type[Rep1Type[F]]) extends Regex[Rep1Type[F]] {
      override final val numCaptures: Int = inner.numCaptures

      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[Rep1Type[F][R]] = {
        inner.sanitiseCode(groups, i)
      }

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[Rep1Type[F][R], C], L, ?] = {
        inner.flattenFunction(nodes, types)
      }
    }

    case class Plus[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using Type[Rep1Type[F]]) extends Rep1[F](inner)
    object Plus {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Plus[F] = {
        given Type[F] = inner.tpe
        new Plus(inner)
      }
    }

    /* {n} for n >= 2. */
    case class Exactly[F[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int)(using Type[Rep1Type[F]]) extends Rep1[F](inner)
    object Exactly {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int)(using Quotes): Exactly[F] = {
        given Type[F] = inner.tpe
        new Exactly(inner, n)
      }
    }

    /* {n,} for n >= 1. Use `Star` for {0,} */
    case class AtLeast[F[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int)(using Type[Rep1Type[F]]) extends Rep1[F](inner)
    object AtLeast {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int)(using Quotes): AtLeast[F] = {
        given Type[F] = inner.tpe
        new AtLeast(inner, n)
      }
    }

    /* {n, m} for m > n >= 1. */
    case class Between[F[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, m: Int)(using Type[Rep1Type[F]]) extends Rep1[F](inner)
    object Between {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, m: Int)(using Quotes): Between[F] = {
        given Type[F] = inner.tpe
        new Between(inner, n, m)
      }
    }

    type Rep0Type[F[_ <: Rep] <: HChain] = OptType[Rep1Type[F]]
    sealed abstract class Rep0[F[_ <: Rep] <: HChain] protected (inner: Regex[F])(using override val tpe: Type[Rep0Type[F]]) extends Regex[Rep0Type[F]] {
      override final val numCaptures: Int = inner.numCaptures     

      override final def sanitiseCode[R <: Rep: Type](groups: Expr[Groups], i: Int)(using Quotes): SanitiseExpr[Rep0Type[F][R]] = {
        Opt(Plus(inner)).sanitiseCode(groups, i) // TODO
      }

      override private [AST] final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using Quotes): FlattenFunction[CCons[Rep0Type[F][R], C], L, ?] = {
        Opt(Plus(inner)).flattenFunction(nodes,types) // TODO
      }
    }

    case class Star[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using Type[Rep0Type[F]]) extends Rep0[F](inner)
    object Star {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes) = {
        given Type[F] = inner.tpe
        new Star(inner)
      }
    }

    /* {0, m} for m >= 2. Use `Opt` for {0, 1}. */
    case class AtMost[F[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int)(using Type[Rep0Type[F]]) extends Rep0[F](inner)
    object AtMost {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int)(using Quotes) = {
        given Type[F] = inner.tpe
        new AtMost(inner, n)
      }
    }
  }
}
