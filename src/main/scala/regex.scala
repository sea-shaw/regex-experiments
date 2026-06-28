package experiments

import cats.collections.Diet
import scala.Tuple.Concat

object regex {

  type ToTuple[A] <: Tuple = A match {
    case EmptyTuple => EmptyTuple
    case a *: as    => a *: as
    case Unit       => EmptyTuple
    case _          => Tuple1[A]
  }

  type FromTuple[A] = A match {
    case EmptyTuple => Unit
    case Tuple1[a] => a
    case _         => A
  }

  type OptCapture[A] = A match {
    case Unit => Unit
    case _    => Option[A]
  }

  type AltCapture[A, B] = (A, B) match {
    case (Unit, Unit) => Unit
    case (a, b)       => Either[a, b]
  }

  sealed trait Regex[A] {
    def get: A = ???
    def unapply(s: String): Option[A] = ???
  }

  case class Lit(c: Int) extends Regex[Unit]

  case class Class(cs: Diet[Int]) extends Regex[Unit]

  case object Dot extends Regex[Unit]

  case class Opt[A](r: Regex[A]) extends Regex[OptCapture[FromTuple[A]]]

  case class Cat[A , B](r1: Regex[A], r2: Regex[B]) extends Regex[FromTuple[Concat[ToTuple[A], ToTuple[B]]]]

  case class Alt[A , B](r1: Regex[A], r2: Regex[B]) extends Regex[AltCapture[FromTuple[A], FromTuple[B]]]

  case class Capture[A](r: Regex[A]) extends Regex[FromTuple[String *: ToTuple[A]]]

  private val tests: Unit = {
    val cap = Capture(Dot)
    val y = cap.get
    val regex = Cat(Cat(Alt(cap, cap), Alt(Dot, Dot)), Cat(Opt(cap), Opt(Dot)))
    val x = regex.get
  }
}
