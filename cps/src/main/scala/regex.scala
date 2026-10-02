package experiments.cps

import experiments.cps.tidy.{Tidy, TidyNonEmpty}

object ast {
  sealed trait TupleTag[T <: Tuple]
  case object EmptyTag extends TupleTag[EmptyTuple]
  case class NonEmptyTag[T <: NonEmptyTuple]() extends TupleTag[T]

  sealed trait RList[T <: Tuple](val tag: TupleTag[T]) {
    def +:[H <: Tuple](reg: Regex[H]): RCons[H, T] = reg.tag match {
      case EmptyTag      => RConsEmpty(reg, this)
      case NonEmptyTag() => RConsNonEmpty(reg, this)
    }
  }
  case object RNil extends RList[EmptyTuple](EmptyTag)
  case class RConsEmpty[T <: Tuple](head: Regex[EmptyTuple], tail: RList[T]) extends RList[T](tail.tag)
  case class RConsNonEmpty[H <: NonEmptyTuple, T <: Tuple](head: Regex[H], tail: RList[T]) extends RList[TidyNonEmpty[H] *: T](NonEmptyTag())

  type RCons[H <: Tuple, T <: Tuple] = H match {
    case EmptyTuple    => RConsEmpty[T]
    case NonEmptyTuple => RConsNonEmpty[H, T]
  }

  sealed trait Regex[A <: Tuple](val tag: TupleTag[A])
  case class Lit(c: Char) extends Regex[EmptyTuple](EmptyTag)
  case class Cat[A <: Tuple](regs: RList[A]) extends Regex[A](regs.tag)
  case class Capture[A <: Tuple](reg: Regex[A]) extends Regex[String *: A](NonEmptyTag())
  case class Opt[A <: Tuple](reg: Regex[A]) extends Regex[Tuple1[Option[Tidy[A]]]](NonEmptyTag())
  case class Alt[A <: Tuple, B <: Tuple](left: Regex[A], right: Regex[B]) extends Regex[Tuple1[Either[Tidy[A], Tidy[B]]]](NonEmptyTag())
}
