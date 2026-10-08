package experiments.cps.macros

import experiments.cps.macros.stack.*
import scala.quoted.Quotes

object tape {
  sealed trait Tape[T <: Tuple] {
    def toStack(using Quotes): Stack[T]
  }

  case object Empty extends Tape[EmptyTuple] {
    override def toStack(using Quotes): Stack[EmptyTuple] = SNil
  }

  case class Cell[Ins <: Tuple, Outs <: Tuple](op: Op[Ins, Outs], tape: Tape[Ins]) extends Tape[Outs] {
    override def toStack(using Quotes): Stack[Outs] = op.toStack(tape)
  }

  sealed trait Op[Ins <: Tuple, Outs <: Tuple] {
    def toStack(tape: Tape[Ins])(using Quotes): Stack[Outs]
  }

  case class Push[A, T <: Tuple](code: Code[A]) extends Op[T, A *: T] {
    override def toStack(tape: Tape[T])(using Quotes): Stack[A *: T] = SCons(code, tape.toStack)
  }

  case class Reduce[A, B, C, T <: Tuple](f: (Code[A], Code[B]) => Quotes ?=> Code[C]) extends Op[B *: A *: T, C *: T] {
    override def toStack(tape: Tape[B *: A *: T])(using Quotes): Stack[C *: T] = tape.toStack match {
      case SCons(b, SCons(a, tail)) => SCons(f(a, b), tail)
    }
  }

  case class Drop[A, T <: Tuple]() extends Op[A *: T, T] {
    override def toStack(tape: Tape[A *: T])(using Quotes): Stack[T] = tape.toStack match {
      case SCons(_, tail) => tail
    }
  }

  case class Apply[A, B, T <: Tuple](f: Code[A] => Quotes ?=> Code[B]) extends Op[A *: T, B *: T] {
    override def toStack(tape: Tape[A *: T])(using Quotes): Stack[B *: T] = tape.toStack match {
      case SCons(a, tail) => SCons(f(a), tail)
    }
  }
}
