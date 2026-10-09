package experiments.cps.macros

import experiments.cps.macros.context.Ctx
import experiments.cps.macros.pos.{Pos, PosExpr, PosInt}
import experiments.cps.macros.stack.*
import experiments.cps.macros.tape.*
import experiments.cps.tidy.*
import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

object state {
  type Res[R <: Tuple] = Option[Tidy[R]]
  case class WithPos[A](res: A, pos: Int)

  sealed trait State[T <: Tuple, R <: Tuple] {
    def go(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[Res[R]]
    def goPos(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[WithPos[Res[R]]]
  }

  case class Accept[R <: Tuple: {TupleTag as tag, Type}]() extends State[TCons[R, EmptyTuple], R] {
    override def go(tape: Tape[TCons[R, EmptyTuple]], pos: Pos)(using Ctx)(using Quotes): Expr[Res[R]] = {
      tag match {
        case EmptyTag     => '{ Some(()) }
        case NonEmptyTag() => tape.toStack match {
          case SCons(code, SNil) => '{ Some(${ code.toExpr }) }
        }
      }
    }

    override def goPos(tape: Tape[TCons[R, EmptyTuple]], pos: Pos)(using Ctx)(using Quotes): Expr[WithPos[Res[R]]] = {
      tag match {
        case EmptyTag      => '{ WithPos(Some(()), ${ pos.toExpr }) }
        case NonEmptyTag() => tape.toStack match {
          case SCons(code, SNil) => '{ WithPos(Some(${ code.toExpr }), ${ pos.toExpr }) }
        }
      }
    }
  }

  case class Item[T <: Tuple, R <: Tuple: Type](c: Char, next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos)(fail: Expr[A])(continue: (Tape[T], Pos) => Ctx ?=> Quotes ?=> Expr[A])(using ctx: Ctx)(using Quotes): Expr[A] = {
      '{
        if (${ pos.toExpr } < ${ ctx.s }.length && ${ ctx.s }.charAt(${ pos.toExpr }) == ${ Expr(c) }) {
          ${ continue(tape, pos + 1) }
        } else $fail
      }
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos)('{ None })(next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos)('{ WithPos(None, -1) })(next.goPos)
    }
  }

  case class Begin[T <: Tuple, R <: Tuple](n: Int, next: State[T, R]) extends State[T, R] {
    private def goWith[A](tape: Tape[T], pos: Pos)(continue: (Tape[T], Pos) => Ctx ?=> Quotes ?=> Expr[A])(using ctx: Ctx)(using Quotes): Expr[A] = {
      continue(tape, pos)(using ctx.withStart(n, pos))
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos)(next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos)(next.goPos)
    }
  }

  case class End[T <: Tuple, R <: Tuple](n: Int, next: State[String *: T, R]) extends State[T, R] {
    private def goWith[A](tape: Tape[T], pos: Pos)(continue: (Tape[String *: T], Pos) => Ctx ?=> Quotes ?=> Expr[A])(using ctx: Ctx)(using Quotes): Expr[A] = {
      val cap = '{ ${ ctx.s }.substring(${ ctx.start(n).toExpr }, ${ pos.toExpr }) }
      continue(Cell(Push(CodeExpr(cap)), tape), pos)
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos)(next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos)(next.goPos)
    }
  }

  case class Split[T <: Tuple, R <: Tuple: Type](left: State[T, R], right: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[Res[R]] = {
      '{ ${ left.go(tape, pos) } orElse ${ right.go(tape, pos) } }
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[WithPos[Res[R]]] = {
      '{
        val leftRes = ${ left.goPos(tape, pos) }
        if leftRes.res.isDefined then leftRes else ${ right.goPos(tape, pos) }
      }
    }
  }

  case class Output[Ins <: Tuple, Outs <: Tuple, R <: Tuple](op: Op[Ins, Outs], next: State[Outs, R]) extends State[Ins, R] {
    override def go(tape: Tape[Ins], pos: Pos)(using Ctx)(using Quotes): Expr[Res[R]] = {
      next.go(Cell(op, tape), pos)
    }

    override def goPos(tape: Tape[Ins], pos: Pos)(using Ctx)(using Quotes): Expr[WithPos[Res[R]]] = {
      next.goPos(Cell(op, tape), pos)
    }
  }

  case class Eof[T <: Tuple, R <: Tuple: Type](next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx)(using Quotes): Expr[Res[R]] = {
      '{ if ${ pos.toExpr } == ${ ctx.s }.length then ${ next.go(tape, pos) } else None }
    }

    override def goPos(tape: Tape[T], pos: Pos)(using ctx: Ctx)(using Quotes): Expr[WithPos[Res[R]]] = {
      '{ if ${ pos.toExpr } == ${ ctx.s }.length then ${ next.goPos(tape, pos) } else WithPos(None, -1) }
    }
  }

  extension [R <: Tuple] (state: State[EmptyTuple, R]) {
    def run(s: Expr[String])(using Quotes): Expr[Res[R]] = state.go(Empty, PosInt(0))(using Ctx.empty(s))
  }

  case class Loop[H <: Tuple: Type, T <: Tuple, R <: Tuple: Type](elem: State[EmptyTuple, H], next: State[List[Tidy[H]] *: T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos)(fail: Expr[A])(goNext: (Tape[List[Tidy[H]] *: T], Pos) => Ctx ?=> Quotes ?=> Expr[A])(using ctx: Ctx)(using Quotes): Expr[A] = {
      '{
        @tailrec
        def loop(pos: Int, acc: List[Tidy[H]]): WithPos[List[Tidy[H]]] = {
          val withPos = ${ elem.goPos(Empty, PosExpr('pos))(using Ctx.empty(ctx.s)) }
          withPos.res match {
            case None      => WithPos(acc.reverse, pos)
            case Some(res) => if withPos.pos == pos then WithPos(acc.reverse, pos) else loop(withPos.pos, res :: acc)
          }
        }

        val res = loop(${ pos.toExpr }, Nil)
        if res.pos >= 0 then ${ goNext(Cell(Push(CodeExpr('{ res.res })), tape), PosExpr('{ res.pos })) } else $fail
      }
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos)('{ None })(next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx)(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos)('{ WithPos(None, -1) })(next.goPos)
    }
  }
}
