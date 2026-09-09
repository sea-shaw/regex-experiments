package experiments.macros

import scala.quoted.Expr

object sanitised {
  type SanitiseExpr[A] = Expr[SanitisedT[A]]

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

  opaque type SanitisedT[A] = Option[Sanitised[A]]

  extension [A] (x: SanitisedT[A]) {
    def value: Option[Sanitised[A]] = x
    def map[B](f: A => B): SanitisedT[B] = x.map(_.map(f))
    def flatMap[B](f: A => SanitisedT[B]): SanitisedT[B] = x.flatMap { a =>
      f(a.captures).map(b => Sanitised(b.captures, a.any || b.any))
    }
    infix def max(y: SanitisedT[A]): SanitisedT[A] = x match {
      case None => y
      case Some(Sanitised(_, any)) => y match {
        case None => x
        case _    => if any then x else y
      }
    }
  }

  extension [A] (x: Option[Sanitised[A]]) {
    def sequence: Sanitised[Option[A]] = x match {
      case None                           => Sanitised(None, false)
      case Some(Sanitised(captures, any)) => Sanitised(Some(captures), any)
    }
  }

  object SanitisedT {
    def apply[A](value: Option[Sanitised[A]]): SanitisedT[A] = value
  }
}
