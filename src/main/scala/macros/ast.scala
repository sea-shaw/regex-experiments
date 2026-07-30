package experiments.macros

import cats.{Applicative, Eval, Functor, Monad, Traverse}
import cats.collections.Diet
import cats.data.{Ior, State}
import cats.kernel.Order
import cats.syntax.all.*
import experiments.macros.evidence.{apply, liftCo}
import experiments.macros.hcollections.hchain.{HAppend, HChain, HConcat, HCons, HEmpty, HSingleton}
import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

object ast {

  type Rep = Boolean & Singleton

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

  private def headExpr[A: Type, B <: HChain: Type](hcons: Expr[HCons[A, B]])(using Quotes): Expr[A] = {
    val x = Expr.summon[HCons[A, B] =:= HSingleton[A]].map { ev =>
      '{ ${ ev(hcons) }.value }
    } orElse Expr.summon[HCons[A, B] =:= HAppend[HSingleton[A], B]].map { ev =>
      '{ ${ ev(hcons) }.left.value }
    }

    x.get
  }

  private def tailExpr[A: Type, B <: HChain: Type](hcons: Expr[HCons[A, B]])(using Quotes): Expr[B] = {
    val x = Expr.summon[HEmpty =:= B].map { ev =>
      ev('{ HChain.nil })
    } orElse Expr.summon[HCons[A, B] =:= HAppend[HSingleton[A], B]].map { ev =>
      '{ ${ ev(hcons) }.right }
    }

    x.get
  }

  private def leftExpr[A <: HChain: Type, B <: HChain: Type](hconcat: Expr[HConcat[A, B]])(using Quotes): Expr[A] = {
    val x = Expr.summon[HEmpty =:= A].map { ev =>
      ev('{ HChain.nil })
    } orElse Expr.summon[HConcat[A, B] =:= A].map { ev =>
      ev(hconcat)
    } orElse Expr.summon[HConcat[A, B] =:= HAppend[A, B]].map { ev =>
      '{ ${ ev(hconcat) }.left }
    }

    x.get
  }

  private def rightExpr[A <: HChain: Type, B <: HChain: Type](hconcat: Expr[HConcat[A, B]])(using Quotes): Expr[B] = {
    val x = Expr.summon[HEmpty =:= B].map { ev =>
      ev('{ HChain.nil })
    } orElse Expr.summon[HConcat[A, B] =:= B].map { ev =>
      ev(hconcat)
    } orElse Expr.summon[HConcat[A, B] =:= HAppend[A, B]].map { ev =>
      '{ ${ ev(hconcat) }.right }
    }

    x.get
  }

  sealed trait AST {
    type InclusiveOr[+_, +_]
    def inclusiveOrType(using Quotes): Type[InclusiveOr]
    def fromOptions[A: Type, B: Type](left: Expr[Option[A]], right: Expr[Option[B]])(using Quotes): Expr[Option[InclusiveOr[A, B]]]
    def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(x: Expr[InclusiveOr[A, B]])(using Quotes): Expr[InclusiveOr[C, D]]

    // TODO: Make private
    sealed trait Nodes {
      type ToChains <: Chains

      def flattenFunction[L <: Leaves](using Quotes): FlattenFunction[ToChains, L, ?]
    }
    type NNil = NNil.type
    case object NNil extends Nodes {
      type ToChains = CNil

      override def flattenFunction[L <: Leaves](using Quotes): FlattenFunction[CNil, L, ?] = ???
    }
    case class NCons[F[_ <: Rep] <: HChain, R <: Rep: Type, N <: Nodes](head: Regex[F], tail: N) extends Nodes {
      type ToChains = CCons[F[R], tail.ToChains]

      override def flattenFunction[L <: Leaves](using Quotes): FlattenFunction[CCons[F[R], tail.ToChains], L, ?] = {
        head.flattenFunction[N, L, R](tail)
      }
    }

    sealed trait Chains
    type CNil = CNil.type
    case object CNil extends Chains
    case class CCons[A <: HChain, C <: Chains](head: Expr[A], tail: C) extends Chains

    sealed trait Leaves
    type LNil = LNil.type
    case object LNil extends Leaves
    case class LCons[A, L <: Leaves](head: Expr[A], tail: L) extends Leaves

    sealed abstract class TidyFunction[-A <: HChain, B](val tpe: Type[B]) {
      def apply(xs: Expr[A])(using Quotes): Expr[B]
    }

    sealed trait FlattenFunction[-C <: Chains, -L <: Leaves, B](val tpe: Type[B]) {
      def apply(chains: C, leaves: L)(using Quotes): Expr[B]
    }

    sealed trait Regex[F[_ <: Rep] <: HChain] {
      val tpe: Type[F]
      def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[F[R]]]
      def flattenFunction[N <: Nodes, L <: Leaves, R <: Rep: Type](nodes: N)(using Quotes): FlattenFunction[CCons[F[R], nodes.ToChains], L, ?]
    }

    type BaseType = Const[HEmpty]
    sealed trait Base extends Regex[BaseType] {
      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[HEmpty]] = {
        State.pure(empty)
      }

      override def flattenFunction[N <: Nodes, L <: Leaves, R <: Rep: Type](nodes: N)(using Quotes): FlattenFunction[CCons[HEmpty, nodes.ToChains], L, ?] = {
        val flatten: FlattenFunction[nodes.ToChains, L, ?] = nodes.flattenFunction[L]
        type A = flatten.tpe.Underlying
        new FlattenFunction[CCons[HEmpty, nodes.ToChains], L, A](flatten.tpe) {
          override def apply(chains: CCons[HEmpty, nodes.ToChains], leaves: L)(using Quotes): Expr[A] = {
            flatten(chains.tail, leaves)
          }
        }
      }
    }

    case class Dot private ()(override val tpe: Type[BaseType]) extends Base
    object Dot {
      def apply()(using Quotes): Dot = {
        new Dot()(Type.of[BaseType])
      }
    }

    case class Lit private (c: Int)(override val tpe: Type[BaseType]) extends Base
    object Lit {
      def apply(c: Int)(using Quotes): Lit = {
        new Lit(c)(Type.of[BaseType])
      }
    }

    case class Class private (cs: Diet[Int])(override val tpe: Type[BaseType]) extends Base
    object Class {
      def apply(cs: Diet[Int])(using Quotes): Class = {
        new Class(cs)(Type.of[BaseType])
      }
    }

    type CaptureType[F[_ <: Rep] <: HChain] = [R <: Rep] =>> HCons[String, F[R]]
    case class Capture[F[_ <: Rep] <: HChain] private (inner: Regex[F])(override val tpe: Type[CaptureType[F]]) extends Regex[CaptureType[F]] {
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

      override def flattenFunction[N <: Nodes, L <: Leaves, R <: Rep: Type](nodes: N)(using Quotes): FlattenFunction[CCons[HCons[String, F[R]], nodes.ToChains], L, ?] = {
        val flatten: FlattenFunction[CCons[F[R], nodes.ToChains], LCons[String, L], ?] = inner.flattenFunction[N, LCons[String, L], R](nodes)
        type A = flatten.tpe.Underlying
        given Type[A] = flatten.tpe
        given Type[F] = inner.tpe
        new FlattenFunction[CCons[HCons[String, F[R]], nodes.ToChains], L, A](flatten.tpe) {
          override def apply(chains: CCons[HCons[String, F[R]], nodes.ToChains], leaves: L)(using Quotes): Expr[A] = {
            '{
              val chain = ${ chains.head }
              val capture = ${ headExpr('chain) }
              val inner = ${ tailExpr('chain) }
              ${ flatten(CCons('{ inner }, chains.tail), LCons('{ capture }, leaves)) }
            }
          }
        }
      }
    }

    object Capture {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Capture[F] = {
        given Type[F] = inner.tpe
        new Capture(inner)(Type.of[CaptureType[F]])
      }
    }

    case class NonCapture[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Regex[F] {
      override val tpe: Type[F] = inner.tpe

      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[F[R]]] = {
        inner.sanitiseCode(groups)
      }

      override def flattenFunction[N <: Nodes, L <: Leaves, R <: Rep: Type](nodes: N)(using Quotes): FlattenFunction[CCons[F[R], nodes.ToChains], L, ?] = {
        inner.flattenFunction[N, L, R](nodes)
      }
    }

    type OptType[F[_ <: Rep] <: HChain] = [R <: Rep] =>> OptCapture[F[R]] 

    case class Opt[F[_ <: Rep] <: HChain] private (inner: Regex[F])(override val tpe: Type[OptType[F]]) extends Regex[OptType[F]] {
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

      override def flattenFunction[N <: Nodes, L <: Leaves, R <: Rep: Type](nodes: N)(using Quotes): FlattenFunction[CCons[OptCapture[F[R]], nodes.ToChains], L, ?] = {
        ???
      }
    }

    object Opt {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Opt[F] = {
        given Type[F] = inner.tpe
        new Opt(inner)(Type.of[OptType[F]])
      }
    }

    type CatType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] = [R <: Rep] =>> HConcat[F[R], G[R]]
    case class Cat[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(override val tpe: Type[CatType[F, G]]) extends Regex[CatType[F, G]] {
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

      override def flattenFunction[N <: Nodes, L <: Leaves, R <: Rep: Type](nodes: N)(using Quotes): FlattenFunction[CCons[HConcat[F[R], G[R]], nodes.ToChains], L, ?] = {
        val flatten: FlattenFunction[Nothing, L, ?] = NCons(left, NCons(right, nodes)).flattenFunction[L]
        type A = flatten.tpe.Underlying
        given Type[A] = flatten.tpe
        given Type[F] = left.tpe
        given Type[G] = right.tpe

        new FlattenFunction[CCons[HConcat[F[R], G[R]], nodes.ToChains], L, A](flatten.tpe) {
          override def apply(chains: CCons[HConcat[F[R], G[R]], nodes.ToChains], leaves: L)(using Quotes): Expr[A] = {
            ???
          }
        }
      }
    }

    object Cat {
      def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G])(using Quotes): Cat[F, G] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe
        new Cat(left, right)(Type.of[CatType[F, G]])
      }
    }

    type AltType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] = [R <: Rep] =>> AltCapture[F[R], G[R], R, InclusiveOr]
    case class Alt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(override val tpe: Type[AltType[F, G]]) extends Regex[AltType[F, G]] {
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

      override def flattenFunction[N <: Nodes, L <: Leaves, R <: Rep: Type](nodes: N)(using Quotes): FlattenFunction[CCons[AltCapture[F[R], G[R], R, InclusiveOr], nodes.ToChains], L, ?] = {
        ???
      }
    }

    object Alt {
      def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G])(using Quotes): Alt[F, G] = {
        given Type[F] = left.tpe
        given Type[G] = right.tpe
        given Type[InclusiveOr] = inclusiveOrType
        new Alt(left, right)(Type.of[AltType[F, G]])
      }
    }

    object Rep0 {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes) = Opt(Rep1(inner))
    }

    /*
    type Rep0Type[F[_ <: Rep] <: HChain] = OptType[Rep1Type[F]]
    class Rep0[F[_ <: Rep] <: HChain] private (inner: Regex[F])(override val tpe: Type[Rep0Type[F]]) extends Regex[Rep0Type[F]] {
      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[Rep0Type[F][R]]] = {
        Opt(Rep1(inner)).sanitiseCode(groups)
      }
    }

    object Rep0 {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Rep0[F] = {
        given Type[F] = inner.tpe
        new Rep0(inner)(Type.of[Rep0Type[F]])
      }
    }
    */

    type Rep1Type[F[_ <: Rep] <: HChain] = Const[F[true]]
    case class Rep1[F[_ <: Rep] <: HChain] private (inner: Regex[F])(override val tpe: Type[Rep1Type[F]]) extends Regex[Rep1Type[F]] {
      override def sanitiseCode[R <: Rep: Type](groups: Expr[Groups])(using Quotes): State[Int, SanitiseExpr[Rep1Type[F][R]]] = {
        inner.sanitiseCode(groups)
      }

      override def flattenFunction[N <: Nodes, L <: Leaves, R <: Rep: Type](nodes: N)(using Quotes): FlattenFunction[CCons[F[true], nodes.ToChains], L, ?] = {
        inner.flattenFunction(nodes)
      }
    }

    object Rep1 {
      def apply[F[_ <: Rep] <: HChain](inner: Regex[F])(using Quotes): Rep1[F] = {
        given Type[F] = inner.tpe
        new Rep1(inner)(Type.of[Rep1Type[F]])
      }
    }
  }

  object Oregano extends AST {
    type InclusiveOr[+A, +B] = Either[Either[A, B], (A, B)]

    override def inclusiveOrType(using Quotes): Type[InclusiveOr] = Type.of[InclusiveOr]

    override def fromOptions[A: Type, B: Type](left: Expr[Option[A]], right: Expr[Option[B]])(using Quotes): Expr[Option[InclusiveOr[A, B]]] = '{
      Ior.fromOptions($left, $right).map(_.unwrap)
    }

    override def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(x: Expr[InclusiveOr[A, B]])(using Quotes): Expr[InclusiveOr[C, D]] = '{
      val left = (x: A) => ${ f('x) }
      val right = (x: B) => ${ g('x) }
      $x.bimap(_.bimap(left, right), _.bimap(left, right))
    }
  }

  object Catnip extends AST {
    type InclusiveOr = Ior

    override def inclusiveOrType(using Quotes): Type[InclusiveOr] = Type.of[InclusiveOr]

    override def fromOptions[A: Type, B: Type](left: Expr[Option[A]], right: Expr[Option[B]])(using Quotes): Expr[Option[InclusiveOr[A, B]]] = '{
      Ior.fromOptions($left, $right)
    }

    override def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(x: Expr[Ior[A, B]])(using Quotes): Expr[InclusiveOr[C, D]] = '{
      $x.bimap(x => ${ f('x) }, x => ${ g('x) })
    }
  }
}
