package experiments.cps

import cats.data.Chain
import experiments.cps.tidy.TCons

object state {
  sealed trait HList[T <: Tuple]
  case object HNil extends HList[EmptyTuple]
  case class HCons[H, T <: Tuple](head: H, tail: HList[T]) extends HList[H *: T]

  sealed trait Op[Ins <: Tuple, Outs <: Tuple]
  case class Push[A, T <: Tuple](x: A) extends Op[T, A *: T]
  case class Reduce[A, B, C, T <: Tuple](reduce: (A, B) => C) extends Op[B *: A *: T, C *: T]
  case class Drop[A, T <: Tuple]() extends Op[A *: T, T]
  case class Apply[A, B, T <: Tuple](f: A => B) extends Op[A *: T, B *: T]

  sealed trait Tape[T <: Tuple]
  case object Empty extends Tape[EmptyTuple]
  case class Cell[Ins <: Tuple, Outs <: Tuple](op: Op[Ins, Outs], tape: Tape[Ins]) extends Tape[Outs]

  sealed trait State[T <: Tuple, R <: Tuple] {
    final def run(s: String): Option[List[String]] = go(s, 0, Chain.nil, Map.empty)
    def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]]
  }

  case class Accept[R <: Tuple]() extends State[TCons[R, EmptyTuple], R] {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = Some(caps.toList)
  }

  case class Item[T <: Tuple, R <: Tuple](c: Char, next: State[T, R]) extends State[T, R] {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = if s.charAt(i) == c then next.go(s, i + 1, caps, starts) else None
  }

  case class Begin[T <: Tuple, R <: Tuple](n: Int, next: State[T, R]) extends State[T, R] {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = next.go(s, i, caps, starts.updated(n, i))
  }

  case class End[T <: Tuple, R <: Tuple](n: Int, next: State[String *: T, R]) extends State[T, R] {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = next.go(s, i, s.substring(starts(n), i) +: caps, starts)
  }

  case class Split[T <: Tuple, R <: Tuple](left: State[T, R], right: State[T, R]) extends State[T, R] {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = left.go(s, i, caps, starts) orElse right.go(s, i, caps, starts)
  }

  case class Output[Ins <: Tuple, Outs <: Tuple, R <: Tuple](op: Op[Ins, Outs], next: State[Outs, R]) extends State[Ins, R] {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = ???
  }
}
