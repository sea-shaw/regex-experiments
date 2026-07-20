package experiments.macros

import scala.quoted.{Expr, Quotes, Type}

object evidence {
  extension [From: Type, To: Type] (ev: Expr[From =:= To])(using Quotes) {
    def substituteBoth[F[_ <: From | To, _ <: From | To]: Type](ftf: Expr[F[To, From]]): Expr[F[From, To]] = ftf.asExprOf[F[From, To]]

    def substituteCo[F[_ <: From | To]: Type](ff: Expr[F[From]]): Expr[F[To]] = {
      type G[_, T <: From | To] = F[T]
      substituteBoth[G](ff)
    }

    def substituteContra[F[_ <: From | To]: Type](ft: Expr[F[To]]): Expr[F[From]] = {
      type G[T <: From | To, _] = F[T]
      substituteBoth[G](ft)
    }

    def apply(f: Expr[From]): Expr[To] = {
      type Id[X <: From | To] = X
      substituteCo[Id](f)
    }

    def flip: Expr[To =:= From] = {
      type G[T <: From | To, F <: From | To] = F =:= T
      substituteBoth[G](ev)
    }

    def liftCo[F[_ <: From | To]: Type]: Expr[F[From] =:= F[To]] = {
      type G[T <: From | To] = F[T] =:= F[To]
      substituteContra[G]('{ summon[G[To]] })
    }

    def liftContra[F[_ <: From | To]: Type]: Expr[F[To] =:= F[From]] = liftCo[F].flip
  }
}
