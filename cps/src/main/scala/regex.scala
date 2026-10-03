package experiments.cps

import experiments.cps.tidy.{Tidy, TidyNonEmpty}

object regex {
  sealed trait TupleTag[T <: Tuple]
  case object EmptyTag extends TupleTag[EmptyTuple]
  case class NonEmptyTag[T <: NonEmptyTuple]() extends TupleTag[T]

  sealed trait RList[T <: Tuple](val tag: TupleTag[T]) {
    def +:[H <: Tuple](reg: Reg[H]): RCons[H, T] = reg.tag match {
      case EmptyTag      => RConsEmpty(reg, this)
      case NonEmptyTag() => RConsNonEmpty(reg, this)
    }
  }
  case object RNil extends RList[EmptyTuple](EmptyTag)
  case class RConsEmpty[T <: Tuple](head: Reg[EmptyTuple], tail: RList[T]) extends RList[T](tail.tag)
  case class RConsNonEmpty[H <: NonEmptyTuple, T <: Tuple](head: Reg[H], tail: RList[T]) extends RList[TidyNonEmpty[H] *: T](NonEmptyTag())

  type RCons[H <: Tuple, T <: Tuple] = H match {
    case EmptyTuple    => RConsEmpty[T]
    case NonEmptyTuple => RConsNonEmpty[H, T]
  }

  sealed trait Reg[A <: Tuple](val tag: TupleTag[A])
  case class Lit(c: Char) extends Reg[EmptyTuple](EmptyTag)
  case class Cat[A <: Tuple](regs: RList[A]) extends Reg[A](regs.tag)
  case class Cap[A <: Tuple](reg: Reg[A]) extends Reg[String *: A](NonEmptyTag())
  case class Opt[A <: Tuple](reg: Reg[A]) extends Reg[Tuple1[Option[Tidy[A]]]](NonEmptyTag())
  case class Alt[A <: Tuple, B <: Tuple](left: Reg[A], right: Reg[B]) extends Reg[Tuple1[Either[Tidy[A], Tidy[B]]]](NonEmptyTag())
}
