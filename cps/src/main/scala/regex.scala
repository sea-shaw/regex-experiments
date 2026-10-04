package experiments.cps

import experiments.cps.state.*
import experiments.cps.tidy.{Tidy, TidyNonEmpty}

object regex {
  sealed trait TupleTag[T <: Tuple]
  case object EmptyTag extends TupleTag[EmptyTuple]
  case class NonEmptyTag[T <: NonEmptyTuple]() extends TupleTag[T]

  sealed trait RList[T <: Tuple](val tag: TupleTag[T]) {
    val numCaps: Int
  
    final def +:[H <: Tuple](reg: Reg[H]): RCons[H, T] = reg.tag match {
      case EmptyTag      => RConsEmpty(reg, this)
      case NonEmptyTag() => RConsNonEmpty(reg, this)
    }

    def cps(next: State, i: Int): State
  }

  case object RNil extends RList[EmptyTuple](EmptyTag) {
    override val numCaps: Int = 0
    override def cps(next: State, i: Int): State = next
  }

  case class RConsEmpty[T <: Tuple](head: Reg[EmptyTuple], tail: RList[T]) extends RList[T](tail.tag) {
    override val numCaps: Int = tail.numCaps
    override def cps(next: State, i: Int): State = head.cps(tail.cps(next, i + head.numCaps), i)
  }

  case class RConsNonEmpty[H <: NonEmptyTuple, T <: Tuple](head: Reg[H], tail: RList[T]) extends RList[TidyNonEmpty[H] *: T](NonEmptyTag()) {
    override val numCaps: Int = head.numCaps + tail.numCaps
    override def cps(next: State, i: Int): State = head.cps(tail.cps(next, i + head.numCaps), i)
  }

  type RCons[H <: Tuple, T <: Tuple] = H match {
    case EmptyTuple    => RConsEmpty[T]
    case NonEmptyTuple => RConsNonEmpty[H, T]
  }

  sealed trait Reg[A <: Tuple](val tag: TupleTag[A]) {
    val numCaps: Int
    final def compile: State = cps(Accept, 0)
    def cps(next: State, i: Int): State
  }

  case class Lit(c: Char) extends Reg[EmptyTuple](EmptyTag) {
    override val numCaps: Int = 0
    override def cps(next: State, i: Int): State = Item(c, next)
  }

  case class Cat[A <: Tuple](regs: RList[A]) extends Reg[A](regs.tag) {
    override val numCaps: Int = regs.numCaps
    override def cps(next: State, i: Int): State = regs.cps(next, i)
  }

  case class CapEmpty(reg: Reg[EmptyTuple]) extends Reg[Tuple1[String]](NonEmptyTag()) {
    override val numCaps: Int = 1
    override def cps(next: State, i: Int): State = Begin(i, reg.cps(End(i, next), i + 1))
  }

  case class CapNonEmpty[A <: NonEmptyTuple](reg: Reg[A]) extends Reg[Tuple2[String, TidyNonEmpty[A]]](NonEmptyTag()) {
    override val numCaps: Int = 1 + reg.numCaps
    override def cps(next: State, i: Int): State = Begin(i, reg.cps(End(i, next), i + 1))
  }

  case class Opt[A <: Tuple](reg: Reg[A]) extends Reg[Tuple1[Option[Tidy[A]]]](NonEmptyTag()) {
    override val numCaps: Int = reg.numCaps
    override def cps(next: State, i: Int): State = Split(reg.cps(next, i), next)
  }

  case class Alt[A <: Tuple, B <: Tuple](left: Reg[A], right: Reg[B]) extends Reg[Tuple1[Either[Tidy[A], Tidy[B]]]](NonEmptyTag()) {
    override val numCaps: Int = left.numCaps + right.numCaps
    override def cps(next: State, i: Int): State = Split(left.cps(next, i), right.cps(next, i + left.numCaps))
  }
}
