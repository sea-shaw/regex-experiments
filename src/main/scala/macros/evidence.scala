package experiments.macros

import scala.quoted.{Expr, Quotes, Type}

object evidence {
  extension [From: Type, To: Type] (ev: Expr[From =:= To])(using Quotes) {
    def substituteBoth[A >: From | To: Type, F[_ <: A, _ <: A]: Type](ftf: Expr[F[To, From]]): Expr[F[From, To]] = ftf.asExprOf[F[From, To]]

    def substituteCo[A >: From | To: Type, F[_ <: A]: Type](ff: Expr[F[From]]): Expr[F[To]] = {
      type G[_, T <: A] = F[T]
      substituteBoth[A, G](ff)
    }

    def substituteContra[A >: From | To: Type, F[_ <: A]: Type](ft: Expr[F[To]]): Expr[F[From]] = {
      type G[T <: A, _] = F[T]
      substituteBoth[A, G](ft)
    }

    def apply(f: Expr[From]): Expr[To] = {
      type Id[X <: From | To] = X
      substituteCo[From | To, Id](f)
    }

    def flip: Expr[To =:= From] = {
      type G[T <: From | To, F <: From | To] = F =:= T
      substituteBoth[From | To, G](ev)
    }

    def liftCo[A >: From | To: Type, F[_ <: A]: Type]: Expr[F[From] =:= F[To]] = {
      type G[T <: A] = F[T] =:= F[To]
      substituteContra[A, G]('{ summon[G[To]] })
    }

    def liftContra[A >: From | To: Type, F[_ <: A]: Type]: Expr[F[To] =:= F[From]] = liftCo[A, F].flip
  }
}
