package experiments.macros

import cats.{Applicative, Eval, Functor, Monad, Traverse}
import cats.kernel.Order
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.quoted.Expr

object sanitised {
  type SanitiseExpr[A] = Expr[SanitisedT[Option, A]]

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

  opaque type SanitisedT[F[_], A] = F[Sanitised[A]]

  extension [F[_], A] (sanitisedT: SanitisedT[F, A]) {
    def value: F[Sanitised[A]] = sanitisedT
  }

  object SanitisedT {

    def apply[F[_], A](value: F[Sanitised[A]]): SanitisedT[F, A] = value

    given [F[_]: Functor as F] => Functor[[A] =>> SanitisedT[F, A]] {
      override def map[A, B](fa: SanitisedT[F, A])(f: A => B): SanitisedT[F, B] = {
        F.map(fa)(_.map(f))
      }
    }

    given [F[_]: Applicative as F] => Applicative[[A] =>> SanitisedT[F, A]] {
      override def pure[A](x: A): SanitisedT[F, A] = {
        F.pure(Applicative[Sanitised].pure(x))
      }

      override def ap[A, B](ff: SanitisedT[F, A => B])(fa: SanitisedT[F, A]): SanitisedT[F, B] = {
        F.map2(ff, fa)(_ ap _)
      }
    }

    given [F[_]: Monad as F] => Monad[[A] =>> SanitisedT[F, A]] {
      override def pure[A](x: A): SanitisedT[F, A] = {
        F.pure(Monad[Sanitised].pure(x))
      }

      override def flatMap[A, B](fa: SanitisedT[F, A])(f: A => SanitisedT[F, B]): SanitisedT[F, B] = {
        F.flatMap(fa) { a =>
          F.map(f(a.captures))(b => Sanitised(b.captures, a.any || b.any))
        }
      }

      override def tailRecM[A, B](a: A)(f: A => SanitisedT[F, Either[A, B]]): SanitisedT[F, B] = {
        F.tailRecM((a, false)) { (x, any) =>
          F.map(f(x)) {
            case Sanitised(Left(left), leftAny)    => Left(left, any || leftAny)
            case Sanitised(Right(right), rightAny) => Right(Sanitised(right, any || rightAny))
          }
        }
      }
    }

    given [F[_], A] => (order: Order[F[Sanitised[A]]]) => Order[SanitisedT[F, A]] = order
  }
}
