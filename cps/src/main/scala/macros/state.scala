package experiments.cps.macros

import experiments.cps.macros.context.{Ctx, JoinPoint}
import experiments.cps.macros.pos.{Pos, PosExpr, PosInt}
import experiments.cps.macros.stack.*
import experiments.cps.macros.tape.*
import experiments.cps.tidy.*
import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

object state {
  type Res[R <: Tuple] = Option[Tidy[R]]
  case class WithPos[R <: Tuple](res: Res[R], pos: Int)

  sealed trait State[T <: Tuple, R <: Tuple] {
    def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]]
    def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]]
  }

  case class Accept[R <: Tuple: {TupleTag as tag, Type}]() extends State[TCons[R, EmptyTuple], R] {
    override def go(tape: Tape[TCons[R, EmptyTuple]], pos: Pos)(using ctx: Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      tag match {
        case EmptyTag     => '{ Some(()) }
        case NonEmptyTag() => tape.toStack match {
          case SCons(code, SNil) => '{ Some(${ code.toExpr }) }
        }
      }
    }

    override def goPos(tape: Tape[TCons[R, EmptyTuple]], pos: Pos)(using Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      tag match {
        case EmptyTag     => '{ WithPos(Some(()), ${ pos.toExpr }) }
        case NonEmptyTag() => tape.toStack match {
          case SCons(code, SNil) => '{ WithPos(Some(${ code.toExpr }), ${ pos.toExpr }) }
        }
      }
    }
  }

  case class Item[T <: Tuple, R <: Tuple: Type](c: Char, next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      '{ 
        if (${ pos.toExpr } < ${ ctx.s }.length && ${ ctx.s }.charAt(${ pos.toExpr }) == ${ Expr(c) }) {
          ${ next.go(tape, pos + 1) }
        } else None
      }
    }

    override def goPos(tape: Tape[T], pos: Pos)(using ctx: Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      '{ 
        if (${ pos.toExpr } < ${ ctx.s }.length && ${ ctx.s }.charAt(${ pos.toExpr }) == ${ Expr(c) }) {
          ${ next.goPos(tape, pos + 1) }
        } else WithPos(None, -1)
      }
    }
  }

  case class Begin[T <: Tuple, R <: Tuple](n: Int, next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      next.go(tape, pos)(using ctx.withStart(n, pos))
    }

    override def goPos(tape: Tape[T], pos: Pos)(using ctx: Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      next.goPos(tape, pos)(using ctx.withStart(n, pos))
    }
  }

  case class End[T <: Tuple, R <: Tuple](n: Int, next: State[String *: T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      val cap = '{ ${ ctx.s }.substring(${ ctx.start(n).toExpr }, ${ pos.toExpr }) }
      next.go(Cell(Push(CodeExpr(cap)), tape), pos)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using ctx: Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      val cap = '{ ${ ctx.s }.substring(${ ctx.start(n).toExpr }, ${ pos.toExpr }) }
      next.goPos(Cell(Push(CodeExpr(cap)), tape), pos)
    }
  }

  case class Split[T <: Tuple, R <: Tuple: Type](left: State[T, R], right: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      '{ ${ left.go(tape, pos) } orElse ${ right.go(tape, pos) } }
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      '{
        val leftRes = ${ left.goPos(tape, pos) }
        if leftRes.res.isDefined then leftRes else ${ right.goPos(tape, pos) }
      }
    }
  }

  case class Output[Ins <: Tuple, Outs <: Tuple, R <: Tuple](op: Op[Ins, Outs], next: State[Outs, R]) extends State[Ins, R] {
    override def go(tape: Tape[Ins], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      next.go(Cell(op, tape), pos)
    }

    override def goPos(tape: Tape[Ins], pos: Pos)(using Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      next.goPos(Cell(op, tape), pos)
    }
  }

  case class MkJoin[H <: Tuple: {TupleTag as tag, Type}, T <: Tuple, R <: Tuple: Type](joinPoint: JoinPoint[H], binder: State[TCons[H, T], R], receiver: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      def newCtx(qjoin: (Pos, Code[Tidy[H]]) => Quotes ?=> Expr[Res[R]]): Ctx[Res[R]] = {
        ctx.withBinding(joinPoint, qjoin)
      }

      tag match {
        case EmptyTag => '{
          def join(pos: Int): Res[R] = ${
            binder.go(tape, PosExpr('pos))(using newCtx((pos, _) => '{ join(${ pos.toExpr }) }))
          }
          ${ receiver.go(tape, pos)(using newCtx((pos, _) => '{ join(${ pos.toExpr }) })) }
        }
        case NonEmptyTag() => '{
          def join(pos: Int, res: TidyNonEmpty[H]): Res[R] = ${ 
            binder.go(Cell(Push(CodeExpr('res)), tape), PosExpr('pos))(using newCtx((pos, res) => '{ join(${ pos.toExpr }, ${ res.toExpr }) }))
          }
          ${ receiver.go(tape, pos)(using newCtx((pos, res) => '{ join(${ pos.toExpr }, ${ res.toExpr }) })) }
        }
      }
    }

    override def goPos(tape: Tape[T], pos: Pos)(using ctx: Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      def newCtx(qjoin: (Pos, Code[Tidy[H]]) => Quotes ?=> Expr[WithPos[R]]): Ctx[WithPos[R]] = {
        ctx.withBinding(joinPoint, qjoin)
      }

      tag match {
        case EmptyTag => '{
          def join(pos: Int): WithPos[R] = ${
            binder.goPos(tape, PosExpr('pos))(using newCtx((pos, _) => '{ join(${ pos.toExpr }) }))
          }
          ${ receiver.goPos(tape, pos)(using newCtx((pos, _) => '{ join(${ pos.toExpr }) })) }
        }
        case NonEmptyTag() => '{
          def join(pos: Int, res: TidyNonEmpty[H]): WithPos[R] = ${ 
            binder.goPos(Cell(Push(CodeExpr('res)), tape), PosExpr('pos))(using newCtx((pos, res) => '{ join(${ pos.toExpr }, ${ res.toExpr }) }))
          }
          ${ receiver.goPos(tape, pos)(using newCtx((pos, res) => '{ join(${ pos.toExpr }, ${ res.toExpr }) })) }
        }
      }
    }
  }

  case class Join[H <: Tuple: TupleTag as tag, T <: Tuple, R <: Tuple](joinPoint: JoinPoint[H]) extends State[TCons[H, T], R] {
    override def go(tape: Tape[TCons[H, T]], pos: Pos)(using ctx: Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      val qjoin = ctx.binding(joinPoint)
      tag match {
        case EmptyTag      => qjoin(pos, Code.unit)
        case NonEmptyTag() => tape.toStack match {
          case SCons(head, _) => qjoin(pos, head)
        }
      }
    }

    override def goPos(tape: Tape[TCons[H, T]], pos: Pos)(using ctx: Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      val qjoin = ctx.binding(joinPoint)
      tag match {
        case EmptyTag      => qjoin(pos, Code.unit)
        case NonEmptyTag() => tape.toStack match {
          case SCons(head, _) => qjoin(pos, head)
        }
      }
    }
  }

  case class Eof[T <: Tuple, R <: Tuple: Type](next: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      '{ if ${ pos.toExpr } == ${ ctx.s }.length then ${ next.go(tape, pos) } else None }
    }

    override def goPos(tape: Tape[T], pos: Pos)(using ctx: Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      '{ if ${ pos.toExpr } == ${ ctx.s }.length then ${ next.goPos(tape, pos) } else WithPos(None, -1) }
    }
  }

  extension [R <: Tuple] (state: State[EmptyTuple, R]) {
    def run(s: Expr[String])(using Quotes): Expr[Res[R]] = state.go(Empty, PosInt(0))(using Ctx.empty(s))
  }

  case class Loop[H <: Tuple: Type, T <: Tuple, R <: Tuple: Type](elem: State[EmptyTuple, H], next: State[List[Tidy[H]] *: T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      '{
        @tailrec
        def loop(pos: Int, acc: List[Tidy[H]]): Res[R] = {
          val withPos = ${ elem.goPos(Empty, PosExpr('pos))(using Ctx.empty(ctx.s)) }
          withPos.res match {
            case None      => continue(pos, acc)
            case Some(res) => if withPos.pos == pos then continue(pos, acc) else loop(withPos.pos, res :: acc)
          }
        }

        def continue(pos: Int, acc: List[Tidy[H]]): Res[R] = ${ next.go(Cell(Push(CodeExpr('{ acc.reverse })), tape), PosExpr('pos)) }

        loop(${ pos.toExpr }, Nil)
      }
    }

    override def goPos(tape: Tape[T], pos: Pos)(using ctx: Ctx[WithPos[R]])(using Quotes): Expr[WithPos[R]] = {
      '{
        @tailrec
        def loop(pos: Int, acc: List[Tidy[H]]): WithPos[R] = {
          val withPos = ${ elem.goPos(Empty, PosExpr('pos))(using Ctx.empty(ctx.s)) }
          withPos.res match {
            case None      => continue(pos, acc)
            case Some(res) => if withPos.pos == pos then continue(pos, acc) else loop(withPos.pos, res :: acc)
          }
        }

        def continue(pos: Int, acc: List[Tidy[H]]): WithPos[R] = ${ next.goPos(Cell(Push(CodeExpr('{ acc.reverse })), tape), PosExpr('pos)) }

        loop(${ pos.toExpr }, Nil)
      }
    }
  }
}
