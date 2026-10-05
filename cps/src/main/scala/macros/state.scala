package experiments.cps.macros

import experiments.cps.macros.qlist.*
import experiments.cps.tidy.*
import scala.quoted.{Expr, Quotes, Type}

object state {
  sealed trait Op[Ins <: Tuple, Outs <: Tuple] {
    def toStack(tape: Tape[Ins])(using Quotes): QList[Outs]
  }

  case class Push[A, T <: Tuple](expr: Expr[A]) extends Op[T, A *: T] {
    override def toStack(tape: Tape[T])(using Quotes): QList[A *: T] = QCons(expr, tape.toStack)
  }

  case class Reduce[A, B, C, T <: Tuple](f: (Expr[A], Expr[B]) => Quotes ?=> Expr[C]) extends Op[B *: A *: T, C *: T] {
    override def toStack(tape: Tape[B *: A *: T])(using Quotes): QList[C *: T] = tape.toStack match {
      case QCons(b, QCons(a, tail)) => QCons(f(a, b), tail)
    }
  }

  case class Drop[A, T <: Tuple]() extends Op[A *: T, T] {
    override def toStack(tape: Tape[A *: T])(using Quotes): QList[T] = tape.toStack match {
      case QCons(_, tail) => tail
    }
  }

  case class Apply[A, B, T <: Tuple](f: Expr[A] => Quotes ?=> Expr[B]) extends Op[A *: T, B *: T] {
    override def toStack(tape: Tape[A *: T])(using Quotes): QList[B *: T] = tape.toStack match {
      case QCons(a, tail) => QCons(f(a), tail)
    }
  }

  sealed trait Tape[T <: Tuple] {
    def toStack(using Quotes): QList[T]
  }

  case object Empty extends Tape[EmptyTuple] {
    override def toStack(using Quotes): QList[EmptyTuple] = QNil
  }

  case class Cell[Ins <: Tuple, Outs <: Tuple](op: Op[Ins, Outs], tape: Tape[Ins]) extends Tape[Outs] {
    override def toStack(using Quotes): QList[Outs] = op.toStack(tape)
  }

  sealed trait State[T <: Tuple, R <: Tuple: Type] {
    def go(tape: Tape[T], s: Expr[String], i: Int, starts: Map[Int, Expr[Int]])(using Quotes): Expr[Option[Tidy[R]]]
  }

  case class Accept[R <: Tuple: {TupleTag as tag, Type}]() extends State[TCons[R, EmptyTuple], R] {
    override def go(tape: Tape[TCons[R, EmptyTuple]], s: Expr[String], i: Int, starts: Map[Int, Expr[Int]])(using Quotes): Expr[Option[Tidy[R]]] = {
      tag match {
        case EmptyTag => '{ if ${ Expr(i) } == $s.length then Some(()) else None }
        case NonEmptyTag() => tape.toStack match {
          case QCons(expr, QNil) => '{ if ${ Expr(i) } == $s.length then Some($expr) else None }
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
      next.go(Cell(Push(cap), tape), s, i, starts)
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
}
