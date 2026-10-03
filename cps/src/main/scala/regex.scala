package experiments.cps

import cats.data.Chain
import experiments.cps.state.*
import experiments.cps.tidy.{Tidy, TidyNonEmpty}

object regex {
  sealed trait TupleTag[T <: Tuple]
  case object EmptyTag extends TupleTag[EmptyTuple]
  case class NonEmptyTag[T <: NonEmptyTuple]() extends TupleTag[T]

  sealed trait RList[T <: Tuple](val tag: TupleTag[T]) {
    val numCaps: Int
    def +:[H <: Tuple](reg: Reg[H]): RCons[H, T] = reg.tag match {
      case EmptyTag      => RConsEmpty(reg, this)
      case NonEmptyTag() => RConsNonEmpty(reg, this)
    }
  }
  case object RNil extends RList[EmptyTuple](EmptyTag) {
    override val numCaps: Int = 0
  }
  case class RConsEmpty[T <: Tuple](head: Reg[EmptyTuple], tail: RList[T]) extends RList[T](tail.tag) {
    override val numCaps: Int = tail.numCaps
  }
  case class RConsNonEmpty[H <: NonEmptyTuple, T <: Tuple](head: Reg[H], tail: RList[T]) extends RList[TidyNonEmpty[H] *: T](NonEmptyTag()) {
    override val numCaps: Int = head.numCaps + tail.numCaps
  }

  type RCons[H <: Tuple, T <: Tuple] = H match {
    case EmptyTuple    => RConsEmpty[T]
    case NonEmptyTuple => RConsNonEmpty[H, T]
  }

  sealed trait Reg[A <: Tuple](val tag: TupleTag[A]) {
    val numCaps: Int
  }
  case class Lit(c: Char) extends Reg[EmptyTuple](EmptyTag) {
    override val numCaps: Int = 0
  }
  case class Cat[A <: Tuple](regs: RList[A]) extends Reg[A](regs.tag) {
    override val numCaps: Int = regs.numCaps
  }
  case class CapEmpty(reg: Reg[EmptyTuple]) extends Reg[Tuple1[String]](NonEmptyTag()) {
    override val numCaps: Int = 1
  }
  case class CapNonEmpty[A <: NonEmptyTuple](reg: Reg[A]) extends Reg[Tuple2[String, TidyNonEmpty[A]]](NonEmptyTag()) {
    override val numCaps: Int = 1 + reg.numCaps
  }
  case class Opt[A <: Tuple](reg: Reg[A]) extends Reg[Tuple1[Option[Tidy[A]]]](NonEmptyTag()) {
    override val numCaps: Int = reg.numCaps
  }
  case class Alt[A <: Tuple, B <: Tuple](left: Reg[A], right: Reg[B]) extends Reg[Tuple1[Either[Tidy[A], Tidy[B]]]](NonEmptyTag()) {
    override val numCaps: Int = left.numCaps + right.numCaps
  }

  def compile[A <: Tuple](reg: Reg[A]): State = {
    def cps[A <: Tuple](reg: Reg[A], next: State, i: Int): State = reg match {
      case Lit(c)           => Item(c, next)
      case Cat(regs)        => cpsRegs(regs, next, i)
      case CapEmpty(reg)    => Begin(i, cps(reg, End(i, next), i + 1))
      case CapNonEmpty(reg) => Begin(i, cps(reg, End(i, next), i + 1))
      case Opt(reg)         => Split(cps(reg, next, i), next)
      case Alt(left, right) => Split(cps(left, next, i), cps(right, next, i + left.numCaps))
    }

    def cpsRegs[A <: Tuple](regs: RList[A], next: State, i: Int): State = regs match {
      case RNil => next
      case RConsEmpty(head, tail) => cps(head, cpsRegs(tail, next, i + head.numCaps), i)
      case RConsNonEmpty(head, tail) => cps(head, cpsRegs(tail, next, i + head.numCaps), i)
    }

    cps(reg, Accept, 0)
  }

  def run(state: State, s: String): Option[List[String]] = {
    def go(state: State, s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = state match {
      case Accept         => Some(caps.toList)
      case Begin(n, next) => go(next, s, i, caps, starts.updated(n, i))
      case End(n, next)   => go(next, s, i, s.substring(starts(n), i) +: caps, starts)
      case Item(c, next)  => if s.charAt(i) == c then go(next, s, i + 1, caps, starts) else None
      case Split(left, right) => go(left, s, i, caps, starts) orElse go(right, s, i, caps, starts)
    }

    go(state, s, 0, Chain.nil, Map.empty)
  }
}
