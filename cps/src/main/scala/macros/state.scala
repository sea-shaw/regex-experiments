package experiments.cps.macros

import experiments.cps.macros.context.{Binding, Ctx, JoinPoint}
import experiments.cps.macros.pos.{Pos, PosExpr, PosInt}
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
    def go(tape: Tape[T], pos: Pos)(using Ctx[R])(using Quotes): Expr[Option[Tidy[R]]]
  }

  case class Accept[R <: Tuple: {TupleTag as tag, Type}]() extends State[TCons[R, EmptyTuple], R] {
    override def go(tape: Tape[TCons[R, EmptyTuple]], pos: Pos)(using ctx: Ctx[R])(using Quotes): Expr[Option[Tidy[R]]] = {
      tag match {
        case EmptyTag     => '{ if ${ pos.toExpr } == ${ ctx.s }.length then Some(()) else None }
        case NonEmptyTag() => tape.toStack match {
          case SCons(code, SNil) => '{ if ${ pos.toExpr } == $ { ctx.s }.length then Some(${ code.toExpr }) else None }
        }
      }
    }
  }

  case class Item[T <: Tuple, R <: Tuple: Type](c: Char, next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[R])(using Quotes): Expr[Option[Tidy[R]]] = {
      '{ if ${ pos.toExpr } < ${ ctx.s }.length && ${ ctx.s }.charAt(${ pos.toExpr }) == ${ Expr(c) } then ${ next.go(tape, pos + 1) } else None }
    }
  }

  case class Begin[T <: Tuple, R <: Tuple: Type](n: Int, next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[R])(using Quotes): Expr[Option[Tidy[R]]] = {
      next.go(tape, pos)(using ctx.withStart(n, pos))
    }
  }

  case class End[T <: Tuple, R <: Tuple: Type](n: Int, next: State[String *: T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[R])(using Quotes): Expr[Option[Tidy[R]]] = {
      val cap = '{ ${ ctx.s }.substring(${ ctx.start(n).toExpr }, ${ pos.toExpr }) }
      next.go(Cell(Push(CodeExpr(cap)), tape), pos)
    }
  }

  case class Split[T <: Tuple, R <: Tuple: Type](left: State[T, R], right: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using Ctx[R])(using Quotes): Expr[Option[Tidy[R]]] = {
      '{ ${ left.go(tape, pos) } orElse ${ right.go(tape, pos) } }
    }
  }

  case class Output[Ins <: Tuple, Outs <: Tuple, R <: Tuple: Type](op: Op[Ins, Outs], next: State[Outs, R]) extends State[Ins, R] {
    override def go(tape: Tape[Ins], pos: Pos)(using Ctx[R])(using Quotes): Expr[Option[Tidy[R]]] = {
      next.go(Cell(op, tape), pos)
    }
  }

  case class MkJoin[H <: Tuple: {TupleTag as tag, Type}, T <: Tuple, R <: Tuple: Type](joinPoint: JoinPoint[H], binder: State[TCons[H, T], R], receiver: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[R])(using Quotes): Expr[Option[Tidy[R]]] = {
      def newCtx(qjoin: (Pos, Code[Tidy[H]]) => Quotes ?=> Expr[Option[Tidy[R]]]): Ctx[R] = {
        ctx.withBinding(joinPoint, Binding(qjoin))
      }

      tag match {
        case EmptyTag => ???
        case NonEmptyTag() => '{
          def join(i: Int, x: TidyNonEmpty[H]): Option[Tidy[R]] = ${ 
            binder.go(Cell(Push(CodeExpr('x)), tape), PosExpr('i))(using newCtx((j, y) => '{ join(${ j.toExpr }, ${ y.toExpr }) }))
          }
          ${ receiver.go(tape, pos)(using newCtx((j, y) => '{ join(${ j.toExpr }, ${ y.toExpr }) })) }
        }
      }
    }
  }

  case class Join[H <: Tuple: TupleTag as tag, T <: Tuple, R <: Tuple: Type](joinPoint: JoinPoint[H]) extends State[TCons[H, T], R] {
    override def go(tape: Tape[TCons[H, T]], pos: Pos)(using ctx: Ctx[R])(using Quotes): Expr[Option[Tidy[R]]] = {
      val Binding(qjoin) = ctx.binding(joinPoint)
      tag match {
        case EmptyTag      => qjoin(pos, Code.unit)
        case NonEmptyTag() => tape.toStack match {
          case SCons(head, _) => qjoin(pos, head)
        }
      }
    }
  }

  extension [R <: Tuple] (state: State[EmptyTuple, R]) {
    def run(s: Expr[String])(using Quotes): Expr[Option[Tidy[R]]] = state.go(Empty, PosInt(0))(using Ctx.empty(s))
  }
}
