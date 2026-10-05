package experiments.cps.macros

import experiments.cps.macros.stack.*
import experiments.cps.tidy.*
import scala.quoted.{Expr, Quotes, Type}

object state {
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

  sealed trait Tape[T <: Tuple] {
    def toStack(using Quotes): Stack[T]
  }

  case object Empty extends Tape[EmptyTuple] {
    override def toStack(using Quotes): Stack[EmptyTuple] = SNil
  }

  case class Cell[Ins <: Tuple, Outs <: Tuple](op: Op[Ins, Outs], tape: Tape[Ins]) extends Tape[Outs] {
    override def toStack(using Quotes): Stack[Outs] = op.toStack(tape)
  }

  sealed trait State[T <: Tuple, R <: Tuple: Type] {
    def go(tape: Tape[T], s: Expr[String], i: Int, starts: Map[Int, Expr[Int]])(using Quotes): Expr[Option[Tidy[R]]]
  }

  case class Accept[R <: Tuple: {TupleTag as tag, Type}]() extends State[TCons[R, EmptyTuple], R] {
    override def go(tape: Tape[TCons[R, EmptyTuple]], s: Expr[String], i: Int, starts: Map[Int, Expr[Int]])(using Quotes): Expr[Option[Tidy[R]]] = {
      tag match {
        case EmptyTag => '{ if ${ Expr(i) } == $s.length then Some(()) else None }
        case NonEmptyTag() => tape.toStack match {
          case SCons(code, SNil) => '{ if ${ Expr(i) } == $s.length then Some(${ code.toExpr }) else None }
        }
      }
    }
  }

  case class Item[T <: Tuple, R <: Tuple: Type](c: Char, next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], s: Expr[String], i: Int, starts: Map[Int, Expr[Int]])(using Quotes): Expr[Option[Tidy[R]]] = {
      '{ if ${ Expr(i) } < $s.length && $s.charAt(${ Expr(i) }) == ${ Expr(c) } then ${ next.go(tape, s, i + 1, starts) } else None }
    }
  }

  case class Begin[T <: Tuple, R <: Tuple: Type](n: Int, next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], s: Expr[String], i: Int, starts: Map[Int, Expr[Int]])(using Quotes): Expr[Option[Tidy[R]]] = {
      next.go(tape, s, i, starts.updated(n, Expr(i)))
    }
  }

  case class End[T <: Tuple, R <: Tuple: Type](n: Int, next: State[String *: T, R]) extends State[T, R] {
    override def go(tape: Tape[T], s: Expr[String], i: Int, starts: Map[Int, Expr[Int]])(using Quotes): Expr[Option[Tidy[R]]] = {
      val cap = '{ $s.substring(${ starts(n) }, ${ Expr(i) }) }
      next.go(Cell(Push(CodeExpr(cap)), tape), s, i, starts)
    }
  }

  case class Split[T <: Tuple, R <: Tuple: Type](left: State[T, R], right: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], s: Expr[String], i: Int, starts: Map[Int, Expr[Int]])(using Quotes): Expr[Option[Tidy[R]]] = {
      '{ ${ left.go(tape, s, i, starts) } orElse ${ right.go(tape, s, i, starts) } }
    }
  }

  case class Output[Ins <: Tuple, Outs <: Tuple, R <: Tuple: Type](op: Op[Ins, Outs], next: State[Outs, R]) extends State[Ins, R] {
    override def go(tape: Tape[Ins], s: Expr[String], i: Int, starts: Map[Int, Expr[Int]])(using Quotes): Expr[Option[Tidy[R]]] = {
      next.go(Cell(op, tape), s, i, starts)
    }
  }

  extension [R <: Tuple] (state: State[EmptyTuple, R]) {
    def run(s: Expr[String])(using Quotes): Expr[Option[Tidy[R]]] = state.go(Empty, s, 0, Map.empty)
  }
}
