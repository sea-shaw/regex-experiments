package experiments.cps.macros

import experiments.cps.macros.context.{Ctx, JoinPoint, Progress}
import experiments.cps.macros.pos.{Pos, PosExpr, PosInt}
import experiments.cps.macros.stack.*
import experiments.cps.macros.tape.*
import experiments.cps.tidy.*
import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

object state {
  type Res[R <: Tuple] = Option[Tidy[R]]
  type Go[T <: Tuple, A] = (Tape[T], Pos) => Ctx[A] ?=> Quotes ?=> Expr[A]
  case class WithPos[A](res: A, pos: Int)

  sealed trait State[T <: Tuple, R <: Tuple] {
    def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]]
    def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]]
  }

  case class Accept[R <: Tuple: {TupleTag as tag, Type}]() extends State[TCons[R, EmptyTuple], R] {
    private def goWith[A: Type](tape: Tape[TCons[R, EmptyTuple]], succeed: Expr[Tidy[R]] => Quotes ?=> Expr[A])(using Quotes): Expr[A] = {
      tag match {
        case EmptyTag      => succeed('{ () })
        case NonEmptyTag() => tape.toStack match {
          case SCons(code, SNil) => succeed(code.toExpr)
        }
      }
    }

    override def go(tape: Tape[TCons[R, EmptyTuple]], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, expr => '{ Some($expr) })
    }

    override def goPos(tape: Tape[TCons[R, EmptyTuple]], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, expr => '{ WithPos(Some($expr), ${ pos.toExpr }) })
    }
  }

  case class Item[T <: Tuple, R <: Tuple: Type](c: Char, next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], continue: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      '{
        if (${ pos.toExpr } < ${ ctx.s }.length && ${ ctx.s }.charAt(${ pos.toExpr }) == ${ Expr(c) }) {
          ${ continue(tape, pos + 1) }
        } else $fail
      }
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, '{ None }, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, '{ WithPos(None, -1) }, next.goPos)
    }
  }

  case class Begin[T <: Tuple, R <: Tuple](n: Int, next: State[T, R]) extends State[T, R] {
    private def goWith[A](tape: Tape[T], pos: Pos, continue: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      continue(tape, pos)(using ctx.withStart(n, pos))
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, next.goPos)
    }
  }

  case class End[T <: Tuple, R <: Tuple](n: Int, next: State[String *: T, R]) extends State[T, R] {
    private def goWith[A](tape: Tape[T], pos: Pos, continue: Go[String *: T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      val cap = '{ ${ ctx.s }.substring(${ ctx.start(n).toExpr }, ${ pos.toExpr }) }
      continue(Cell(Push(CodeExpr(cap)), tape), pos)
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, next.goPos)
    }
  }

  case class Split[T <: Tuple, R <: Tuple: Type](left: State[T, R], right: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      '{ ${ left.go(tape, pos) } orElse ${ right.go(tape, pos) } }
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
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

    override def goPos(tape: Tape[Ins], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      next.goPos(Cell(op, tape), pos)
    }
  }

  case class MkJoin[H <: Tuple: {TupleTag as tag, Type}, T <: Tuple, R <: Tuple: Type](joinPoint: JoinPoint[H], binder: State[TCons[H, T], R], receiver: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, goBinder: Go[TCons[H, T], A], goReceiver: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      def newCtx(qjoin: (Pos, Code[Tidy[H]]) => Quotes ?=> Expr[A]): Ctx[A] = {
        ctx.withBinding(joinPoint, qjoin)
      }

      tag match {
        case EmptyTag => '{
          def join(pos: Int): A = ${
            goBinder(tape, PosExpr('pos))(using newCtx((pos, _) => '{ join(${ pos.toExpr }) }))
          }
          ${ goReceiver(tape, pos)(using newCtx((pos, _) => '{ join(${ pos.toExpr }) })) }
        }
        case NonEmptyTag() => '{
          def join(pos: Int, res: TidyNonEmpty[H]): A = ${
            goBinder(Cell(Push(CodeExpr('res)), tape), PosExpr('pos))(using newCtx((pos, res) => '{ join(${ pos.toExpr }, ${ res.toExpr }) }))
          }
          ${ goReceiver(tape, pos)(using newCtx((pos, res) => '{ join(${ pos.toExpr }, ${ res.toExpr }) })) }
        }
      }
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, binder.go, receiver.go)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, binder.goPos, receiver.goPos)
    }
  }

  case class Join[H <: Tuple: TupleTag as tag, T <: Tuple, R <: Tuple](joinPoint: JoinPoint[H]) extends State[TCons[H, T], R] {
    private def goWith[A](tape: Tape[TCons[H, T]], pos: Pos)(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      val qjoin = ctx.binding(joinPoint)
      tag match {
        case EmptyTag      => qjoin(pos, Code.unit)
        case NonEmptyTag() => tape.toStack match {
          case SCons(head, _) => qjoin(pos, head)
        }
      }
    }

    override def go(tape: Tape[TCons[H, T]], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos)
    }

    override def goPos(tape: Tape[TCons[H, T]], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos)
    }
  }

  case class Eof[T <: Tuple, R <: Tuple: Type](next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, goNext: Go[T, A], fail: Expr[A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      '{ if ${ pos.toExpr } == ${ ctx.s }.length then ${ goNext(tape, pos) } else $fail }
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, next.go, '{ None })
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, next.goPos, '{ WithPos(None, -1) })
    }
  }

  case class Loop[H <: Tuple: Type, T <: Tuple, R <: Tuple: Type](elem: State[EmptyTuple, H], next: State[List[Tidy[H]] *: T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, isValid: Expr[A] => Quotes ?=> Expr[Boolean], goNext: Go[List[Tidy[H]] *: T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      '{
        @tailrec
        def loop(pos: Int, elems: List[Tidy[H]], acc: List[WithPos[List[Tidy[H]]]]): A = {
          val withPos = ${ elem.goPos(Empty, PosExpr('pos))(using Ctx.empty(ctx.s)) }
          withPos.res match {
            case None => backtrack(acc)
            case Some(elem) => if withPos.pos == pos then backtrack(acc) else {
              val newElems = elem :: elems
              loop(withPos.pos, newElems, WithPos(newElems, withPos.pos) :: acc)
            }
          }
        }

        @tailrec
        def backtrack(acc: List[WithPos[List[Tidy[H]]]]): A = acc match {
          case Nil => continue(${ pos.toExpr }, Nil)
          case WithPos(list, pos) :: tail => {
            val res = continue(pos, list)
            if ${ isValid('res) } then res else backtrack(tail)
          }
        }

        def continue(pos: Int, list: List[Tidy[H]]): A = ${ goNext(Cell(Push(CodeExpr('list)), tape), PosExpr('pos)) }

        loop(${ pos.toExpr }, Nil, Nil)
      }
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, expr => '{ $expr.isDefined }, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, expr => '{ $expr.pos >= 0 }, next.goPos)
    }
  }

  case class SaveProgress[T <: Tuple, R <: Tuple: Type](progress: Progress, next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, goNext: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      goNext(tape, pos)(using ctx.withProgress(progress, pos))
    }

    override def go(tape: Tape[T], pos: Pos)(using ctx: Ctx[Res[R]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, next.go)
    }
  
    override def goPos(tape: Tape[T], pos: Pos)(using ctx: Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Option[Tidy[R]]]] = {
      goWith(tape, pos, next.goPos)
    }
  }

  case class RequireProgress[T <: Tuple, R <: Tuple: Type](progress: Progress, next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, goNext: Go[T, A], fail: Expr[A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      val prev = ctx.progress(progress)
      '{
        if ${ pos.toExpr } > ${ prev.toExpr } then ${ goNext(tape, pos) } else $fail
      }
    }

    override def go(tape: Tape[T], pos: Pos)(using Ctx[Res[R]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, next.go, '{ None })
    }

    override def goPos(tape: Tape[T], pos: Pos)(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Option[Tidy[R]]]] = {
      goWith(tape, pos, next.goPos, '{ WithPos(None, -1) })
    }
  }

  extension [R <: Tuple] (state: State[EmptyTuple, R]) {
    def run(s: Expr[String])(using Quotes): Expr[Res[R]] = state.go(Empty, PosInt(0))(using Ctx.empty(s))
  }
}
