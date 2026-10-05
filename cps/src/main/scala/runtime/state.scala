package experiments.cps.runtime

import experiments.cps.runtime.hlist.*
import experiments.cps.tidy.*

object state {
  sealed trait Op[Ins <: Tuple, Outs <: Tuple] {
    def toStack(tape: Tape[Ins]): HList[Outs]
  }

  case class Push[A, T <: Tuple](x: A) extends Op[T, A *: T] {
    override def toStack(tape: Tape[T]): HList[A *: T] = HCons(x, tape.toStack)
  }

  case class Reduce[A, B, C, T <: Tuple](reduce: (A, B) => C) extends Op[B *: A *: T, C *: T] {
    override def toStack(tape: Tape[B *: A *: T]): HList[C *: T] = tape.toStack match {
      case HCons(b, HCons(a, tail)) => HCons(reduce(a, b), tail)
    }
  }

  case class Drop[A, T <: Tuple]() extends Op[A *: T, T] {
    override def toStack(tape: Tape[A *: T]): HList[T] = tape.toStack match {
      case HCons(_, tail) => tail 
    }
  }

  case class Apply[A, B, T <: Tuple](f: A => B) extends Op[A *: T, B *: T] {
    override def toStack(tape: Tape[A *: T]): HList[B *: T] = tape.toStack match {
      case HCons(head, tail) => HCons(f(head), tail) 
    }
  }

  sealed trait Tape[T <: Tuple] {
    def toStack: HList[T]
  }

  case object Empty extends Tape[EmptyTuple] {
    override def toStack: HList[EmptyTuple] = HNil
  }

  case class Cell[Ins <: Tuple, Outs <: Tuple](op: Op[Ins, Outs], tape: Tape[Ins]) extends Tape[Outs] {
    override def toStack: HList[Outs] = op.toStack(tape)
  }

  sealed trait State[T <: Tuple, R <: Tuple] {
    def go(tape: Tape[T], s: String, i: Int, starts: Map[Int, Int]): Option[Tidy[R]]
  }

  case class Accept[R <: Tuple: TupleTag as tag]() extends State[TCons[R, EmptyTuple], R] {
    override def go(tape: Tape[TCons[R, EmptyTuple]], s: String, i: Int, starts: Map[Int, Int]): Option[Tidy[R]] = {
      if i == s.length then tag match {
        case EmptyTag      => Some(())
        case NonEmptyTag() => tape.toStack match {
          case HCons(x, HNil) => Some(x) 
        }
      } else None
    }
  }

  case class Item[T <: Tuple, R <: Tuple](c: Char, next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], s: String, i: Int, starts: Map[Int, Int]): Option[Tidy[R]] = {
      if i < s.length && s.charAt(i) == c then next.go(tape, s, i + 1, starts) else None
    }
  }

  case class Begin[T <: Tuple, R <: Tuple](n: Int, next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], s: String, i: Int, starts: Map[Int, Int]): Option[Tidy[R]] = {
      next.go(tape, s, i, starts.updated(n, i))
    }
  }

  case class End[T <: Tuple, R <: Tuple](n: Int, next: State[String *: T, R]) extends State[T, R] {
    override def go(tape: Tape[T], s: String, i: Int, starts: Map[Int, Int]): Option[Tidy[R]] = {
      next.go(Cell(Push(s.substring(starts(n), i)), tape), s, i, starts)
    }
  }

  case class Split[T <: Tuple, R <: Tuple](left: State[T, R], right: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], s: String, i: Int, starts: Map[Int, Int]): Option[Tidy[R]] = {
      left.go(tape, s, i, starts) orElse right.go(tape, s, i, starts)
    }
  }

  case class Output[Ins <: Tuple, Outs <: Tuple, R <: Tuple](op: Op[Ins, Outs], next: State[Outs, R]) extends State[Ins, R] {
    override def go(tape: Tape[Ins], s: String, i: Int, starts: Map[Int, Int]): Option[Tidy[R]] = {
      next.go(Cell(op, tape), s, i, starts)
    }
  }

  extension [R <: Tuple] (state: State[EmptyTuple, R]) {
    def run(s: String): Option[Tidy[R]] = state.go(Empty, s, 0, Map.empty)
  }
}
