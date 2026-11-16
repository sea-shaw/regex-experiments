package experiments.cps.macros

import cats.data.Chain
import experiments.cps.macros.context.{Ctx, JoinPoint, Progress}
import experiments.cps.macros.pos.{Pos, PosExpr, PosInt}
import experiments.cps.macros.stack.*
import experiments.cps.macros.tape.*
import experiments.cps.tidy.*
import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

object state {
  type Res[R <: Tuple] = Option[Tidy[R]]
  type Go[T <: Tuple, A] = (Tape[T], Pos, Expr[A]) => Ctx[A] ?=> Quotes ?=> Expr[A]
  case class WithPos[+A](res: A, pos: Int)

  sealed trait State[T <: Tuple, R <: Tuple] {
    def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]]
    def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]]
    def goAll(tape: Tape[T], pos: Pos)(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]]
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

    override def go(tape: Tape[TCons[R, EmptyTuple]], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, expr => '{ Some($expr) })
    }

    override def goPos(tape: Tape[TCons[R, EmptyTuple]], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, expr => '{ WithPos(Some($expr), ${ pos.toExpr }) })
    }

    override def goAll(tape: Tape[TCons[R, EmptyTuple]], pos: Pos)(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, expr => '{ Chain.one(WithPos($expr, ${ pos.toExpr })) })
    }
  }

  case class Item[T <: Tuple, R <: Tuple: Type](c: Char, next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], continue: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      '{
        if (${ pos.toExpr } < ${ ctx.s }.length && ${ ctx.s }.charAt(${ pos.toExpr }) == ${ Expr(c) }) {
          ${ continue(tape, pos + 1, fail) }
        } else $fail
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos)(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      '{
        if (${ pos.toExpr } < ${ ctx.s }.length && ${ ctx.s }.charAt(${ pos.toExpr }) == ${ Expr(c) }) {
          ${ next.goAll(tape, pos + 1) }
        } else Chain.nil
      }
    }
  }

  case class Begin[T <: Tuple, R <: Tuple](n: Int, next: State[T, R]) extends State[T, R] {
    private def goWith[A](tape: Tape[T], pos: Pos, fail: Expr[A], continue: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      continue(tape, pos, fail)(using ctx.withStart(n, pos))
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos)(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      next.goAll(tape, pos)(using ctx.withStart(n, pos))
    }
  }

  case class End[T <: Tuple, R <: Tuple](n: Int, next: State[String *: T, R]) extends State[T, R] {
    private def goWith[A](tape: Tape[T], pos: Pos, fail: Expr[A], continue: Go[String *: T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      val cap = '{ ${ ctx.s }.substring(${ ctx.start(n).toExpr }, ${ pos.toExpr }) }
      continue(Cell(Push(CodeExpr(cap)), tape), pos, fail)
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos)(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      val cap = '{ ${ ctx.s }.substring(${ ctx.start(n).toExpr }, ${ pos.toExpr }) }
      next.goAll(Cell(Push(CodeExpr(cap)), tape), pos)
    }
  }

  case class Split[T <: Tuple, R <: Tuple: Type](left: State[T, R], right: State[T, R]) extends State[T, R] {
    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      '{
        def alt = ${ right.go(tape, pos, fail) }
        ${ left.go(tape, pos, 'alt) }
      }
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      '{
        def alt = ${ right.goPos(tape, pos, fail) }
        ${ left.goPos(tape, pos, 'alt) }
      }
    }

    override def goAll(tape: Tape[T], pos: Pos)(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      '{
        ${ left.goAll(tape, pos) } ++ ${ right.goAll(tape, pos) }
      }
    }
  }

  case class Output[Ins <: Tuple, Outs <: Tuple, R <: Tuple](op: Op[Ins, Outs], next: State[Outs, R]) extends State[Ins, R] {
    override def go(tape: Tape[Ins], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      next.go(Cell(op, tape), pos, fail)
    }

    override def goPos(tape: Tape[Ins], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      next.goPos(Cell(op, tape), pos, fail)
    }

    override def goAll(tape: Tape[Ins], pos: Pos)(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      next.goAll(Cell(op, tape), pos)
    }
  }

  case class MkJoin[H <: Tuple: {TupleTag as tag, Type}, T <: Tuple, R <: Tuple: Type](joinPoint: JoinPoint[H], binder: State[TCons[H, T], R], receiver: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goBinder: Go[TCons[H, T], A], goReceiver: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      def newCtx(qjoin: (Pos, Code[Tidy[H]]) => Quotes ?=> Expr[A]): Ctx[A] = {
        ctx.withBinding(joinPoint, qjoin)
      }

      tag match {
        case EmptyTag => '{
          def join(pos: Int): A = ${
            goBinder(tape, PosExpr('pos), fail)(using newCtx((pos, _) => '{ join(${ pos.toExpr }) }))
          }
          ${ goReceiver(tape, pos, fail)(using newCtx((pos, _) => '{ join(${ pos.toExpr }) })) }
        }
        case NonEmptyTag() => '{
          def join(pos: Int, res: TidyNonEmpty[H]): A = ${
            goBinder(Cell(Push(CodeExpr('res)), tape), PosExpr('pos), fail)(using newCtx((pos, res) => '{ join(${ pos.toExpr }, ${ res.toExpr }) }))
          }
          ${ goReceiver(tape, pos, fail)(using newCtx((pos, res) => '{ join(${ pos.toExpr }, ${ res.toExpr }) })) }
        }
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, fail, binder.go, receiver.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, fail, binder.goPos, receiver.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos)(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, '{ Chain.nil }, (tape, pos, _) => binder.goAll(tape, pos), (tape, pos, _) => receiver.goAll(tape, pos))
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

    override def go(tape: Tape[TCons[H, T]], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos)
    }

    override def goPos(tape: Tape[TCons[H, T]], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos)
    }

    override def goAll(tape: Tape[TCons[H, T]], pos: Pos)(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos)
    }
  }

  case class Eof[T <: Tuple, R <: Tuple: Type](next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goNext: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      '{ if ${ pos.toExpr } == ${ ctx.s }.length then ${ goNext(tape, pos, fail) } else $fail }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos)(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      '{ if ${ pos.toExpr } == ${ ctx.s }.length then ${ next.goAll(tape, pos) } else Chain.nil }
    }
  }

  case class LoopFlat[H <: Tuple: Type, T <: Tuple, R <: Tuple: Type](elem: State[EmptyTuple, H], next: State[List[Tidy[H]] *: T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goNext: Go[List[Tidy[H]] *: T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      '{
        @tailrec
        def loop(pos: Int, elems: List[Tidy[H]], acc: List[WithPos[List[Tidy[H]]]]): A = {
          val withPos = ${ elem.goPos(Empty, PosExpr('pos), '{ WithPos(None, -1) })(using Ctx.empty(ctx.s)) }
          withPos.res match {
            case None       => backtrack(acc)
            case Some(elem) => if withPos.pos == pos then backtrack(acc) else {
              val newElems = elem :: elems
              loop(withPos.pos, newElems, WithPos(newElems, withPos.pos) :: acc)
            }
          }
        }

        @tailrec
        def backtrack(lists: List[WithPos[List[Tidy[H]]]]): A = lists match {
          case Nil                        => $fail
          case WithPos(list, pos) :: tail => ${ goNext(Cell(Push(CodeExpr('list)), tape), PosExpr('pos), '{ backtrack(tail) }) }
        }

        loop(${ pos.toExpr }, Nil, List(WithPos(Nil, ${ pos.toExpr })))
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Res[R]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Res[R]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos)(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      '{
        @tailrec
        def loop(pos: Int, elems: List[Tidy[H]], acc: List[WithPos[List[Tidy[H]]]]): Chain[WithPos[Tidy[R]]] = {
          val withPos = ${ elem.goPos(Empty, PosExpr('pos), '{ WithPos(None, -1) })(using Ctx.empty(ctx.s)) }
          withPos.res match {
            case None => backtrack(acc, Chain.nil)
            case Some(elem) => if withPos.pos == pos then backtrack(acc, Chain.nil) else {
              val newElems = elem :: elems
              loop(withPos.pos, newElems, WithPos(newElems, withPos.pos) :: acc)
            }
          }
        }

        @tailrec
        def backtrack(lists: List[WithPos[List[Tidy[H]]]], acc: Chain[WithPos[Tidy[R]]]): Chain[WithPos[Tidy[R]]] = lists match {
          case Nil                        => acc
          case WithPos(list, pos) :: tail => {
            backtrack(tail, acc ++ ${ next.goAll(Cell(Push(CodeExpr('list)), tape), PosExpr('pos)) })
          }
        }

        loop(${ pos.toExpr }, Nil, List(WithPos(Nil, ${ pos.toExpr })))
      }
    }
  }

  case class LoopNested[H <: Tuple: Type, T <: Tuple, R <: Tuple: Type](elem: State[Tuple1[List[Tidy[H]]], Tuple2[List[Tidy[H]], Boolean]], next: State[List[Tidy[H]] *: T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, goNext: Go[List[Tidy[H]] *: T, A], fail: Expr[A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      '{
        @tailrec
        def loop(lists: List[WithPos[(List[Tidy[H]], Boolean)]]): A = lists match {
          case Nil => $fail
          case WithPos((list, true), pos) :: tail => ${ goNext(Cell(Push(CodeExpr('list)), tape), PosExpr('pos), '{ loop(tail) }) }
          case WithPos((list, false), pos) :: tail => {
            val all = ${ elem.goAll(Cell(Push(CodeExpr('list)), Empty), PosExpr('pos))(using Ctx.empty(ctx.s)) }
            loop(all.iterator ++: (WithPos((list, true), pos) :: tail))
          }
        }

        loop(List(WithPos((Nil, false), ${ pos.toExpr })))
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, next.go, fail)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Option[Tidy[R]]]] = {
      goWith(tape, pos, next.goPos, fail)
    }

    override def goAll(tape: Tape[T], pos: Pos)(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      '{
        @tailrec
        def loop(lists: List[WithPos[(List[Tidy[H]], Boolean)]], acc: Chain[WithPos[Tidy[R]]]): Chain[WithPos[Tidy[R]]] = lists match {
          case Nil => acc
          case WithPos((list, true), pos) :: tail => {
            val results = ${ next.goAll(Cell(Push(CodeExpr('list)), tape), PosExpr('pos)) }
            loop(tail, acc ++ results)
          }
          case WithPos((list, false), pos) :: tail => {
            val all = ${ elem.goAll(Cell(Push(CodeExpr('list)), Empty), PosExpr('pos))(using Ctx.empty(ctx.s)) }
            loop(all.iterator ++: (WithPos((list, true), pos) :: tail), acc)
          }
        }

        loop(List(WithPos((Nil, false), ${ pos.toExpr })), Chain.nil)
      }
    }
  }

  case class SaveProgress[T <: Tuple, R <: Tuple: Type](progress: Progress, next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goNext: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      goNext(tape, pos, fail)(using ctx.withProgress(progress, pos))
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using ctx: Ctx[Res[R]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }
  
    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using ctx: Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Option[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos)(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      next.goAll(tape, pos)(using ctx.withProgress(progress, pos))
    }
  }

  case class RequireProgress[T <: Tuple, R <: Tuple: Type](progress: Progress, next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goNext: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      val prev = ctx.progress(progress)
      '{
        if ${ pos.toExpr } > ${ prev.toExpr } then ${ goNext(tape, pos, fail) } else $fail
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Res[R]])(using Ctx[Res[R]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[WithPos[Res[R]]])(using Ctx[WithPos[Res[R]]])(using Quotes): Expr[WithPos[Option[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos)(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      val prev = ctx.progress(progress)
      '{
        if ${ pos.toExpr } > ${ prev.toExpr } then ${ next.goAll(tape, pos) } else Chain.nil
      }
    }
  }

  extension [R <: Tuple: Type] (state: State[EmptyTuple, R]) {
    def run(s: Expr[String])(using Quotes): Expr[Res[R]] = state.go(Empty, PosInt(0), '{ None })(using Ctx.empty(s))
    def runAll(s: Expr[String])(using Quotes): Expr[List[Tidy[R]]] = '{ ${ state.goAll(Empty, PosInt(0))(using Ctx.empty(s)) }.toList.map(_.res) }
  }
}
