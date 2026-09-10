package experiments.macros

import scala.quoted.Expr

object sanitised {
  type SanitiseExpr[A] = Expr[Option[Sanitised[A]]]

  case class Sanitised[+A](captures: A, any: Boolean) {
    def map[B](f: A => B) = Sanitised(f(captures), any)
    def map2[B](other: Sanitised[B])[C](f: (A, B) => C): Sanitised[C] = {
      Sanitised(f(captures, other.captures), any || other.any)
    }
    def flatMap[B](f: A => Sanitised[B]) = {
      val b = f(captures)
      Sanitised(b.captures, any || b.any)
    }
    def traverse[B](f: A => Option[B]): Option[Sanitised[B]] = {
      f(captures).map(Sanitised(_, any))
    }
  }
}
