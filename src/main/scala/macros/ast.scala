package experiments.macros

import cats.{Applicative, Eval, Functor, Monad, Traverse}
import cats.collections.Diet
import cats.data.State
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
        SanitisedT {
          Monad[F].tailRecM((a, false)) { (x, any) =>
            f(x).value.map {
              case Sanitised(Left(left), leftAny) => Left(left, any || leftAny)
              case Sanitised(Right(right), rightAny) => Right(Sanitised(right, any || rightAny))
            }
          }
        }
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
    '{ Applicative[[A] =>> SanitisedT[Option, A]].pure(HChain.nil) }
  }

  private def liftSanitised[A <: HChain: Type, B <: HChain: Type](ev: Expr[A =:= B])(using Quotes): Expr[SanitisedT[Option, A] =:= SanitisedT[Option, B]] = {
    ev.liftCo[[X <: HChain] =>> SanitisedT[Option, X]]
  }

  trait AST {
    type InclusiveOr[+_, +_]
    protected def inclusiveOrType(using Quotes): Type[InclusiveOr]
    protected def fromOptions[A: Type, B: Type](left: Expr[Option[A]], right: Expr[Option[B]])(using Quotes): Expr[Option[InclusiveOr[A, B]]]
    protected def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(expr: Expr[InclusiveOr[A, B]])(using Quotes): Expr[InclusiveOr[C, D]]
    protected def tconsBuildFunction[A: Type](tail: Types)(using Quotes): BuildFunction[LCons[A, tail.ToLeaves], ?]

    private sealed trait Nodes {
      type ToChains <: Chains

      def flattenFunction(types: Types)(using Quotes): FlattenFunction[ToChains, types.ToLeaves, ?]
    }

    private case object NNil extends Nodes {
      type ToChains = CNil

      override def flattenFunction(types: Types)(using Quotes): FlattenFunction[CNil, types.ToLeaves, ?] = {
        types.buildFunction match {
          case build @ BuildFunction(given Type[a]) => new FlattenFunction[CNil, types.ToLeaves, a] {
            override def apply(chains: CNil, leaves: types.ToLeaves)(using Quotes): Expr[a] = {
              build(leaves)
            }
          }
        }
      }
    }

    private case class NCons[F[_ <: Rep] <: HChain, R <: Rep: Type, N <: Nodes & Singleton](head: Regex[F], tail: N) extends Nodes {
      type ToChains = CCons[F[R], tail.ToChains]

      override def flattenFunction(types: Types)(using Quotes): FlattenFunction[CCons[F[R], tail.ToChains], types.ToLeaves, ?] = {
        head.flattenFunction[R](tail, types)
      }
    }

    private sealed trait Chains
    private type CNil = CNil.type
    private case object CNil extends Chains
    private case class CCons[A <: HChain, C <: Chains](head: Expr[A], tail: C) extends Chains

    protected sealed trait Types {
      type ToLeaves <: Leaves
      def buildFunction(using Quotes): BuildFunction[ToLeaves, ?]
    }

    protected case object TNil extends Types {
      type ToLeaves = LNil

      override def buildFunction(using Quotes): BuildFunction[LNil, ?] = new BuildFunction[LNil, Unit] {
        override def apply(leaves: LNil)(using Quotes): Expr[Unit] = '{ () }
      }
    }

    protected case class TCons[A, T <: Types & Singleton](head: Type[A], tail: T) extends Types {
      type ToLeaves = LCons[A, tail.ToLeaves]
      override def buildFunction(using Quotes): BuildFunction[ToLeaves, ?] = {
        given Type[A] = head
        tconsBuildFunction(tail)
      }
    }

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

      def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[F[R]]]

      final def tidyFunction[R <: Rep: Type](using Quotes): TidyFunction[F[R], ?] = {
        flattenFunction[R](NNil, TNil) match {
          case flatten @ FlattenFunction(given Type[a]) => new TidyFunction[F[R], a] {
            override def apply(xs: Expr[F[R]])(using Quotes): Expr[a] = {
              flatten(CCons(xs, CNil), LNil)
            }
          }
        }
      }

      private [AST] def flattenFunction[R <: Rep: Type](nodes: Nodes, types: Types)(using Quotes): FlattenFunction[CCons[F[R], nodes.ToChains], types.ToLeaves, ?]
    }

    type BaseType = Const[HEmpty]
    sealed abstract class Base extends Regex[BaseType] {
      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[HEmpty]] = {
        State.pure(empty)
      }

      override private [AST] def flattenFunction[R <: Rep: Type](nodes: Nodes, types: Types)(using Quotes): FlattenFunction[CCons[HEmpty, nodes.ToChains], types.ToLeaves, ?] = {
        nodes.flattenFunction(types) match {
          case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HEmpty, nodes.ToChains], types.ToLeaves, a] {
            override def apply(chains: CCons[HEmpty, nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[a] = {
              flatten(chains.tail, leaves)
            }
          }
        }
      }
    }

    case class Dot private ()(using override val tpe: Type[BaseType]) extends Base
    object Dot {
      def apply()(using Quotes): Dot = {
        new Dot()
      }
    }

    case class Lit private (c: Int)(using override val tpe: Type[BaseType]) extends Base
    object Lit {
      def apply(c: Int)(using Quotes): Lit = {
        new Lit(c)
      }
    }

    case class Class private (cs: Diet[Int])(using override val tpe: Type[BaseType]) extends Base
    object Class {
      def apply(cs: Diet[Int])(using Quotes): Class = {
        new Class(cs)
      }
    }

    type CaptureType[F[_ <: Rep] <: HChain] = [R <: Rep] =>> HCons[String, F[R]]
    case class Capture[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using override val tpe: Type[CaptureType[F]]) extends Regex[CaptureType[F]] {
      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[CaptureType[F][R]]] = {
        given Type[F] = inner.tpe

        val capture = State { (i: Int) =>
          val idx = Expr(i)
          val expr = '{
            val sanitised = $groups($idx).map { s =>
              Sanitised(HChain.one(s), true)
            }
            SanitisedT(sanitised)
          }
          (i + 1, expr)
        }

        Expr.summon[HSingleton[String] =:= CaptureType[F][R]].map { ev =>
          capture.map(liftSanitised(ev)(_))
        } getOrElse {
          (capture, inner.sanitiseCode(groups)).mapN { case (sanitisedCapture, sanitisedInner) =>
            '{
              for {
                capture <- $sanitisedCapture
                inner <- $sanitisedInner
              } yield capture ++ inner
            }
          }
        }
      }

      override private [AST] def flattenFunction[R <: Rep: Type](nodes: Nodes, types: Types)(using Quotes): FlattenFunction[CCons[CaptureType[F][R], nodes.ToChains], types.ToLeaves, ?] = {
        given Type[F] = inner.tpe

        val flatten = Expr.summon[CaptureType[F][R] =:= HSingleton[String]].map { ev =>
          nodes.flattenFunction(TCons(Type.of[String], types)) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CaptureType[F][R], nodes.ToChains], types.ToLeaves, a] {
              override def apply(chains: CCons[CaptureType[F][R], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[a] = {
                val capture = '{ ${ ev(chains.head) }.value }
                flatten(chains.tail, LCons(capture, leaves))
              }
            }
          }
        } orElse Expr.summon[CaptureType[F][R] =:= HAppend[HSingleton[String], F[R]]].map { ev =>
          inner.flattenFunction[R](nodes, TCons(Type.of[String], types)) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CaptureType[F][R], nodes.ToChains], types.ToLeaves, a] {
              override def apply(chains: CCons[CaptureType[F][R], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[a] = {
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

    object Capture {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Capture[F] = {
        given Type[F] = inner.tpe
        new Capture(inner)
      }
    }

    case class NonCapture[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Regex[F] {
      override val tpe: Type[F] = inner.tpe

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[F[R]]] = {
        inner.sanitiseCode(groups)
      }

      override private [AST] def flattenFunction[R <: Rep: Type](nodes: Nodes, types: Types)(using Quotes): FlattenFunction[CCons[F[R], nodes.ToChains], types.ToLeaves, ?] = {
        inner.flattenFunction[R](nodes, types)
      }
    }

    type OptType[F[_ <: Rep] <: HChain] = [R <: Rep] =>> OptCapture[F[R]] 

    case class Opt[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using override val tpe: Type[OptType[F]]) extends Regex[OptType[F]] {
      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[OptType[F][R]]] = {
        given Type[F] = inner.tpe

        val sanitised = Expr.summon[HEmpty =:= OptType[F][R]].map { ev =>
          State.pure(liftSanitised(ev)(empty))
        } orElse Expr.summon[HSingleton[Option[F[R]]] =:= OptType[F][R]].map { ev =>
          inner.sanitiseCode(groups).map { sanitisedInner =>
            liftSanitised(ev) {
              '{
                val innerCaps = $sanitisedInner
                SanitisedT(Some(innerCaps.value.sequence.map(HChain.one)))
              }
            }
          }
        }
        sanitised.get
      }

      override private [AST] def flattenFunction[R <: Rep: Type](nodes: Nodes, types: Types)(using Quotes): FlattenFunction[CCons[OptCapture[F[R]], nodes.ToChains], types.ToLeaves, ?] = {
        given Type[F] = inner.tpe
        val flatten = Expr.summon[OptType[F][R] =:= HEmpty].map { _ =>
          nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[OptCapture[F[R]], nodes.ToChains], types.ToLeaves, a] {
              override def apply(chains: CCons[OptCapture[F[R]], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[a] = {
                flatten(chains.tail, leaves)
              }
            }
          }
        } orElse Expr.summon[OptType[F][R] =:= HSingleton[Option[F[R]]]].map { ev =>
          inner.tidyFunction[R] match {
            case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
              case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[OptCapture[F[R]], nodes.ToChains], types.ToLeaves, b] {
                override def apply(chains: CCons[OptCapture[F[R]], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[b] = {
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

    type CatType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] = [R <: Rep] =>> HConcat[F[R], G[R]]
    case class Cat[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(using override val tpe: Type[CatType[F, G]]) extends Regex[CatType[F, G]] {
      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[CatType[F, G][R]]] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe

        Expr.summon[F[R] =:= CatType[F, G][R]].map { ev =>
          left.sanitiseCode(groups).map(liftSanitised(ev)(_))
        } orElse Expr.summon[G[R] =:= CatType[F, G][R]].map { ev =>
          right.sanitiseCode(groups).map(liftSanitised(ev)(_))
        } getOrElse {
          (left.sanitiseCode(groups), right.sanitiseCode(groups)).mapN { case (sanitisedLeft, sanitisedRight) =>
            '{
              for {
                left <- $sanitisedLeft
                right <- $sanitisedRight
              } yield left ++ right
            }
          }
        }
      }

      override private [AST] def flattenFunction[R <: Rep: Type](nodes: Nodes, types: Types)(using Quotes): FlattenFunction[CCons[CatType[F, G][R], nodes.ToChains], types.ToLeaves, ?] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe

        val flatten = Expr.summon[CatType[F, G][R] =:= F[R]].map { ev =>
          left.flattenFunction[R](nodes, types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CatType[F, G][R], nodes.ToChains], types.ToLeaves, a] {
              override def apply(chains: CCons[CatType[F, G][R], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[a] = {
                flatten(CCons(ev(chains.head), chains.tail), leaves)
              }
            }
          }
        } orElse Expr.summon[CatType[F, G][R] =:= G[R]].map { ev =>
          right.flattenFunction[R](nodes, types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CatType[F, G][R], nodes.ToChains], types.ToLeaves, a] {
              override def apply(chains: CCons[CatType[F, G][R], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[a] = {
                flatten(CCons(ev(chains.head), chains.tail), leaves)
              }
            }
          }
        } orElse Expr.summon[CatType[F, G][R] =:= HAppend[F[R], G[R]]].map { ev =>
          left.flattenFunction(NCons(right, nodes), types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CatType[F, G][R], nodes.ToChains], types.ToLeaves, a] {
              override def apply(chains: CCons[CatType[F, G][R], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[a] = {
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
      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[AltType[F, G][R]]] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe
        given Type[InclusiveOr] = inclusiveOrType

        val sanitised = Expr.summon[HEmpty =:= AltType[F, G][R]].map { ev =>
          State.pure(liftSanitised(ev)(empty))
        } orElse Expr.summon[SingletonWith[Either, F[R], G[R]] =:= AltType[F, G][R]].map { ev =>
          (left.sanitiseCode(groups), right.sanitiseCode(groups)).mapN { case (sanitisedLeft, sanitisedRight) =>
            liftSanitised(ev) {
              '{
                val left = $sanitisedLeft.map(_.asLeft[G[R]])
                val right = $sanitisedRight.map(_.asRight[F[R]])
                (left max right).map(HChain.one)
              }
            }
          }
        } orElse Expr.summon[SingletonWith[InclusiveOr, F[R], G[R]] =:= AltType[F, G][R]].map { ev =>
          (left.sanitiseCode(groups), right.sanitiseCode(groups)).mapN { case (sanitisedLeft, sanitisedRight) =>
            liftSanitised(ev) {
              '{
                val left = $sanitisedLeft.value.sequence
                val right = $sanitisedRight.value.sequence
                val caps = (left, right).mapN((l, r) => ${ fromOptions('l, 'r) })
                SanitisedT(caps.traverse(_.map(HChain.one)))
              }
            }
          }
        }

        sanitised.get
      }

      override private [AST] def flattenFunction[R <: Rep: Type](nodes: Nodes, types: Types)(using Quotes): FlattenFunction[CCons[AltType[F, G][R], nodes.ToChains], types.ToLeaves, ?] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe
        given Type[InclusiveOr] = inclusiveOrType

        val flatten = Expr.summon[AltType[F, G][R] =:= HEmpty].map { _ =>
          nodes.flattenFunction(types) match {
            case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[AltType[F, G][R], nodes.ToChains], types.ToLeaves, a] {
              override def apply(chains: CCons[AltType[F, G][R], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[a] = {
                flatten(chains.tail, leaves)
              }
            }
          }
        } orElse Expr.summon[AltType[F, G][R] =:= SingletonWith[Either, F[R], G[R]]].map { ev =>
          (left.tidyFunction[R], right.tidyFunction[R]) match {
            case (tidyLeft @ TidyFunction(given Type[a]), tidyRight @ TidyFunction(given Type[b])) => nodes.flattenFunction(TCons(Type.of[Either[a, b]], types)) match {
              case flatten @ FlattenFunction(given Type[c]) => new FlattenFunction[CCons[AltType[F, G][R], nodes.ToChains], types.ToLeaves, c] {
                override def apply(chains: CCons[AltType[F, G][R], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[c] = {
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
              case flatten @ FlattenFunction(given Type[c]) => new FlattenFunction[CCons[AltType[F, G][R], nodes.ToChains], types.ToLeaves, c] {
                override def apply(chains: CCons[AltType[F, G][R], nodes.ToChains], leaves: types.ToLeaves)(using Quotes): Expr[c] = {
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

    object Rep0 {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes) = Opt(Rep1(inner))
    }

    type Rep1Type[F[_ <: Rep] <: HChain] = Const[F[true]]
    case class Rep1[F[_ <: Rep] <: HChain] private (inner: Regex[F])(using override val tpe: Type[Rep1Type[F]]) extends Regex[Rep1Type[F]] {
      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[Rep1Type[F][R]]] = {
        inner.sanitiseCode(groups)
      }

      override private [AST] def flattenFunction[R <: Rep: Type](nodes: Nodes, types: Types)(using Quotes): FlattenFunction[CCons[F[true], nodes.ToChains], types.ToLeaves, ?] = {
        inner.flattenFunction[true](nodes, types)
      }
    }

    object Rep1 {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Rep1[F] = {
        given Type[F] = inner.tpe
        new Rep1(inner)
      }
    }
  }
}
