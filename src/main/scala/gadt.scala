package experiments

import cats.collections.Diet
import scala.Tuple.Concat

object gadt {

  type ToTuple[A] <: Tuple = A match {
    case EmptyTuple => EmptyTuple
    case a *: as    => a *: as
    case Unit       => EmptyTuple
    case _          => Tuple1[A]
  }

  type FromTuple[A <: Tuple] = A match {
    case EmptyTuple => Unit
    case Tuple1[a] => a
    case _         => A
  }

  type OptCapture[A] = A match {
    case Unit => Unit
    case _    => Option[A]
  }

  type AltCapture[A, B] = A match {
    case Unit => B match {
      case Unit => Unit
    }
    case _ => Either[A, B]
  }

  type CatCapture[A, B] = FromTuple[Concat[ToTuple[A], ToTuple[B]]]

  type CaptureType[A] = FromTuple[String *: ToTuple[A]]

  private def toTuple[A](x: A): ToTuple[A] = x match {
    case _: EmptyTuple => EmptyTuple
    case x: (_ *: _)   => x
    case _: Unit       => EmptyTuple
    case x: Any        => x *: EmptyTuple
  }

  private def fromTuple[A <: Tuple](x: A): FromTuple[A] = x match {
    case _: EmptyTuple => ()
    case x: Tuple1[_]  => x._1
    case x: Any        => x
  }

  sealed trait Regex[A] {
    def unapply(s: String): Option[A] = ???
    def shape: A
  }

  case class Lit(c: Int) extends Regex[Unit] {
    override def shape: Unit = ()
  }

  case class Class(cs: Diet[Int]) extends Regex[Unit] {
    override def shape: Unit = ()

  }

  case object Dot extends Regex[Unit] {
    override def shape: Unit = ()

  }

  case class Opt[A](r: Regex[A]) extends Regex[OptCapture[A]] {
    override def shape: OptCapture[A] = r.shape match {
      case _: Unit => ()
      case x: Any  => Some(x)
    }
  }

  case class Alt[A, B](r1: Regex[A], r2: Regex[B]) extends Regex[AltCapture[A, B]] {
    override def shape: AltCapture[A, B] = r1.shape match {
      case _: Unit => r2.shape match {
        case _: Unit => ()
      }
      case _: Any => Right(r2.shape)
    }
  }

  case class Cat[A, B](r1: Regex[A], r2: Regex[B]) extends Regex[CatCapture[A, B]] {
    override def shape: CatCapture[A, B] = fromTuple(toTuple(r1.shape) ++ toTuple(r2.shape))
  }

  case class Capture[A](r: Regex[A]) extends Regex[CaptureType[A]] {
    override def shape: CaptureType[A] = {
      fromTuple(("" *: toTuple(r.shape)))
    }
  }
}
