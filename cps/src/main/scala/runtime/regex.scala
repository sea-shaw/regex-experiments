package experiments.cps.runtime

import experiments.cps.runtime.state.*
import experiments.cps.tidy.*
import scala.annotation.unused

object regex {
  sealed trait RList[T <: Tuple](using val tag: TupleTag[T]) {
    val numCaps: Int
  
    final def +:[H <: Tuple](reg: Reg[H]): RList[TCons[H, T]] = reg.tag match {
      case EmptyTag      => RConsEmpty(reg, this)
      case NonEmptyTag() => RConsNonEmpty(reg, this)
    }

    def cps[Xs <: Tuple, R <: Tuple](next: State[T *: Xs, R], i: Int): State[Xs, R]
  }

  case object RNil extends RList[EmptyTuple] {
    override val numCaps: Int = 0
    override def cps[Xs <: Tuple, R <: Tuple](next: State[EmptyTuple *: Xs, R], i: Int): State[Xs, R] = {
      Output(Push(EmptyTuple), next)
    }
  }

  case class RConsEmpty[T <: Tuple](head: Reg[EmptyTuple], tail: RList[T]) extends RList[T](using tail.tag) {
    override val numCaps: Int = tail.numCaps
    override def cps[Xs <: Tuple, R <: Tuple](next: State[T *: Xs, R], i: Int): State[Xs, R] = {
      head.cps(tail.cps(next, i + head.numCaps), i)
    }
  }

  case class RConsNonEmpty[H <: NonEmptyTuple, T <: Tuple](head: Reg[H], tail: RList[T]) extends RList[TidyNonEmpty[H] *: T] {
    override val numCaps: Int = head.numCaps + tail.numCaps
  
    override def cps[Xs <: Tuple, R <: Tuple](next: State[(TidyNonEmpty[H] *: T) *: Xs, R], i: Int): State[Xs, R] = {
      head.cps(tail.cps(Output(Reduce(_ *: _), next), i + head.numCaps), i)
    }
  }

  sealed trait Reg[A <: Tuple](using val tag: TupleTag[A]) {  
    val numCaps: Int
  
    final def run(s: String): Option[Tidy[A]] = compile.run(s)
    final def compile: State[EmptyTuple, A] = cps(Accept(), 0)

    def cps[T <: Tuple, R <: Tuple](next: State[TCons[A, T], R], i: Int): State[T, R]
  }

  case class Lit(c: Char) extends Reg[EmptyTuple] {
    override val numCaps: Int = 0
    override def cps[T <: Tuple, R <: Tuple](next: State[T, R], i: Int): State[T, R] = {
      Item(c, next)
    }
  }

  case class Cat[A <: Tuple](regs: RList[A]) extends Reg[A](using regs.tag) {
    override val numCaps: Int = regs.numCaps
    override def cps[T <: Tuple, R <: Tuple](next: State[TCons[A, T], R], i: Int): State[T, R] = {
      val catOp: Op[A *: T, TCons[A, T]] = tag match {
        case EmptyTag      => Drop()
        case NonEmptyTag() => Apply(tidy)
      }
      regs.cps(Output(catOp, next), i)
    }
  }

  case class CapEmpty(reg: Reg[EmptyTuple]) extends Reg[Tuple1[String]] {
    override val numCaps: Int = 1
    override def cps[T <: Tuple, R <: Tuple](next: State[String *: T, R], i: Int): State[T, R] = {
      Begin(i, reg.cps(End(i, next), i + 1))
    }
  }

  case class CapNonEmpty[A <: NonEmptyTuple](reg: Reg[A]) extends Reg[Tuple2[String, TidyNonEmpty[A]]] {
    override val numCaps: Int = 1 + reg.numCaps
    override def cps[T <: Tuple, R <: Tuple](next: State[Tuple2[String, TidyNonEmpty[A]] *: T, R], i: Int): State[T, R] = {
      Begin(i, reg.cps(End(i, Output(Reduce((inner, cap) => (cap, inner)), next)), i + 1))
    }
  }

  case class Opt[A <: Tuple](reg: Reg[A]) extends Reg[Tuple1[Option[Tidy[A]]]] {
    override val numCaps: Int = reg.numCaps
    override def cps[T <: Tuple, R <: Tuple](next: State[Option[Tidy[A]] *: T, R], i: Int): State[T, R] = {
      val someOp = op(reg.tag, Some(_), next)
      Split(reg.cps(Output(someOp, next), i), Output(Push(None), next))
    }
  }

  case class Alt[A <: Tuple, B <: Tuple](left: Reg[A], right: Reg[B]) extends Reg[Tuple1[Either[Tidy[A], Tidy[B]]]] {
    override val numCaps: Int = left.numCaps + right.numCaps
    override def cps[T <: Tuple, R <: Tuple](next: State[Either[Tidy[A], Tidy[B]] *: T, R], i: Int): State[T, R] = {
      val leftOp = op(left.tag, Left(_), next)
      val rightOp = op(right.tag, Right(_), next)
      Split(left.cps(Output(leftOp, next), i), right.cps(Output(rightOp, next), i + left.numCaps))
    }
  }

  private def op[A <: Tuple, B, T <: Tuple](tag: TupleTag[A], f: Tidy[A] => B, @unused next: State[B *: T, ?]): Op[TCons[A, T], B *: T] = tag match {
    case EmptyTag      => Push(f(()))
    case NonEmptyTag() => Apply(f(_))
  }
}
