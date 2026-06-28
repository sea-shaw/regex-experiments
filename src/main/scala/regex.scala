package experiments

import cats.collections.Diet

object regex {

  sealed trait Regex[A <: Tuple] {
    def get: A = ???
    def unapply(s: String): Option[A] = ???
  }

  case class Lit(c: Int) extends Regex[EmptyTuple]

  case class Class(cs: Diet[Int]) extends Regex[EmptyTuple]

  case object Dot extends Regex[EmptyTuple]

  case class Opt[A <: Tuple](r: Regex[A]) extends Regex[Tuple1[Option[A]]]
  
  case class Rep0[A <: Tuple](r: Regex[A]) extends Regex[Tuple1[Option[A]]]

  case class Rep1[A <: Tuple](r: Regex[A]) extends Regex[A]

  case class Cat[A <: Tuple, B <: Tuple](r1: Regex[A], r2: Regex[B]) extends Regex[Tuple.Concat[A, B]]

  case class Alt[A <: Tuple, B <: Tuple](r1: Regex[A], r2: Regex[B]) extends Regex[Tuple1[Either[A, B]]]

  case class Capture[A <: Tuple](r: Regex[A]) extends Regex[String *: A]
}
