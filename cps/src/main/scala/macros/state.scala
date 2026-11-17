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
  type Go[T <: Tuple, A] = (Tape[T], Pos, Expr[A]) => Ctx[A] ?=> Quotes ?=> Expr[A]
  case class WithPos[+A](res: A, pos: Int)

  sealed trait State[T <: Tuple, R <: Tuple] {
    def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]]
    def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]]
    def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]]
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

    override def go(tape: Tape[TCons[R, EmptyTuple]], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, expr => '{ Some($expr) })
    }

    override def goPos(tape: Tape[TCons[R, EmptyTuple]], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, expr => '{ Some(WithPos($expr, ${ pos.toExpr })) })
    }

    override def goAll(tape: Tape[TCons[R, EmptyTuple]], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, expr => '{ WithPos($expr, ${ pos.toExpr }) +: $fail })
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

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goAll)
    }
  }

  case class Begin[T <: Tuple, R <: Tuple](n: Int, next: State[T, R]) extends State[T, R] {
    private def goWith[A](tape: Tape[T], pos: Pos, fail: Expr[A], continue: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      continue(tape, pos, fail)(using ctx.withStart(n, pos))
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goAll)
    }
  }

  case class End[T <: Tuple, R <: Tuple](n: Int, next: State[String *: T, R]) extends State[T, R] {
    private def goWith[A](tape: Tape[T], pos: Pos, fail: Expr[A], continue: Go[String *: T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      val cap = '{ ${ ctx.s }.substring(${ ctx.start(n).toExpr }, ${ pos.toExpr }) }
      continue(Cell(Push(CodeExpr(cap)), tape), pos, fail)
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goAll)
    }
  }

  case class Split[T <: Tuple, R <: Tuple: Type](left: State[T, R], right: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goLeft: Go[T, A], goRight: Go[T, A])(using Ctx[A])(using Quotes): Expr[A] = {
      '{
        def alt = ${ goRight(tape, pos, fail) }
        ${ goLeft(tape, pos, 'alt) }
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, left.go, right.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, left.goPos, right.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, left.goAll, right.goAll)
    }
  }

  case class Output[Ins <: Tuple, Outs <: Tuple, R <: Tuple](op: Op[Ins, Outs], next: State[Outs, R]) extends State[Ins, R] {
    private def goWith[A](tape: Tape[Ins], pos: Pos, fail: Expr[A], goNext: Go[Outs, A])(using Ctx[A])(using Quotes): Expr[A] = {
      goNext(Cell(op, tape), pos, fail)
    }

    override def go(tape: Tape[Ins], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[Ins], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[Ins], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goAll)
    }
  }

  case class MkJoin[H <: Tuple: {TupleTag as tag, Type}, T <: Tuple, R <: Tuple: Type](joinPoint: JoinPoint[H], binder: State[TCons[H, T], R], receiver: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goBinder: Go[TCons[H, T], A], goReceiver: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      def newCtx(qjoin: (Pos, Code[Tidy[H]], Expr[A]) => Quotes ?=> Expr[A]): Ctx[A] = {
        ctx.withBinding(joinPoint, qjoin)
      }

      tag match {
        case EmptyTag => '{
          def join(pos: Int, fail: A): A = ${
            goBinder(tape, PosExpr('pos), 'fail)(using newCtx((pos, _, fail) => '{ join(${ pos.toExpr }, $fail) }))
          }
          ${ goReceiver(tape, pos, fail)(using newCtx((pos, _, fail) => '{ join(${ pos.toExpr }, $fail) })) }
        }
        case NonEmptyTag() => '{
          def join(pos: Int, res: TidyNonEmpty[H], fail: A): A = ${
            goBinder(Cell(Push(CodeExpr('res)), tape), PosExpr('pos), 'fail)(using newCtx((pos, res, fail) => '{ join(${ pos.toExpr }, ${ res.toExpr }, $fail) }))
          }
          ${ goReceiver(tape, pos, fail)(using newCtx((pos, res, fail) => '{ join(${ pos.toExpr }, ${ res.toExpr }, $fail) })) }
        }
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, binder.go, receiver.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, binder.goPos, receiver.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, binder.goAll, receiver.goAll)
    }
  }

  case class Join[H <: Tuple: TupleTag as tag, T <: Tuple, R <: Tuple](joinPoint: JoinPoint[H]) extends State[TCons[H, T], R] {
    private def goWith[A](tape: Tape[TCons[H, T]], pos: Pos, fail: Expr[A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      val qjoin = ctx.binding(joinPoint)
      tag match {
        case EmptyTag      => qjoin(pos, Code.unit, fail)
        case NonEmptyTag() => tape.toStack match {
          case SCons(head, _) => qjoin(pos, head, fail)
        }
      }
    }

    override def go(tape: Tape[TCons[H, T]], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail)
    }

    override def goPos(tape: Tape[TCons[H, T]], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail)
    }

    override def goAll(tape: Tape[TCons[H, T]], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail)
    }
  }

  case class Eof[T <: Tuple, R <: Tuple: Type](next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goNext: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      '{ if ${ pos.toExpr } == ${ ctx.s }.length then ${ goNext(tape, pos, fail) } else $fail }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goAll)
    }
  }

  case class LoopFlat[H <: Tuple: Type, T <: Tuple, R <: Tuple: Type](elem: State[EmptyTuple, H], next: State[List[Tidy[H]] *: T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[Option[A]], goNext: Go[List[Tidy[H]] *: T, Option[A]])(using ctx: Ctx[Option[A]])(using Quotes): Expr[Option[A]] = {
      '{
        @tailrec
        def loop(pos: Int, elems: List[Tidy[H]], acc: List[WithPos[List[Tidy[H]]]]): Option[A] = {
          ${ elem.goPos(Empty, PosExpr('pos), '{ None })(using Ctx.empty(ctx.s)) } match {
            case None                        => backtrack(acc)
            case Some(WithPos(elem, newPos)) => if newPos == pos then backtrack(acc) else {
              val newElems = elem :: elems
              loop(newPos, newElems, WithPos(newElems, newPos) :: acc)
            }
          }
        }

        @tailrec
        def backtrack(lists: List[WithPos[List[Tidy[H]]]]): Option[A] = lists match {
          case Nil                        => $fail
          case WithPos(list, pos) :: tail => {
            val res = ${ goNext(Cell(Push(CodeExpr('list)), tape), PosExpr('pos), '{ None }) }
            if res.isDefined then res else backtrack(tail)
          }
        }

        loop(${ pos.toExpr }, Nil, List(WithPos(Nil, ${ pos.toExpr })))
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      '{
        @tailrec
        def loop(pos: Int, elems: List[Tidy[H]], acc: List[WithPos[List[Tidy[H]]]]): Chain[WithPos[Tidy[R]]] = {
          ${ elem.goPos(Empty, PosExpr('pos), '{ None })(using Ctx.empty(ctx.s)) } match {
            case None                        => backtrack(acc, $fail)
            case Some(WithPos(elem, newPos)) => if newPos == pos then backtrack(acc, $fail) else {
              val newElems = elem :: elems
              loop(newPos, newElems, WithPos(newElems, newPos) :: acc)
            }
          }
        }

        @tailrec
        def backtrack(lists: List[WithPos[List[Tidy[H]]]], acc: Chain[WithPos[Tidy[R]]]): Chain[WithPos[Tidy[R]]] = lists match {
          case Nil                        => acc
          case WithPos(list, pos) :: tail => {
            backtrack(tail, acc ++ ${ next.goAll(Cell(Push(CodeExpr('list)), tape), PosExpr('pos), '{ Chain.nil }) })
          }
        }

        loop(${ pos.toExpr }, Nil, List(WithPos(Nil, ${ pos.toExpr })))
      }
    }
  }

  case class LoopNested[H <: Tuple: Type, T <: Tuple, R <: Tuple: Type](elem: State[Tuple1[List[Tidy[H]]], Tuple2[List[Tidy[H]], Boolean]], next: State[List[Tidy[H]] *: T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[Option[A]], goNext: Go[List[Tidy[H]] *: T, Option[A]])(using ctx: Ctx[Option[A]])(using Quotes): Expr[Option[A]] = {
      '{
        @tailrec
        def loop(lists: List[WithPos[(List[Tidy[H]], Boolean)]]): Option[A] = lists match {
          case Nil => $fail
          case WithPos((list, true), pos) :: tail => {
            val res = ${ goNext(Cell(Push(CodeExpr('list)), tape), PosExpr('pos), '{ None }) }
            if res.isDefined then res else loop(tail)
          }
          case WithPos((list, false), pos) :: tail => {
            val all = ${ elem.goAll(Cell(Push(CodeExpr('list)), Empty), PosExpr('pos), '{ Chain.nil })(using Ctx.empty(ctx.s)) }
            loop(all.iterator ++: (WithPos((list, true), pos) :: tail))
          }
        }

        loop(List(WithPos((Nil, false), ${ pos.toExpr })))
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using ctx: Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      '{
        @tailrec
        def loop(lists: List[WithPos[(List[Tidy[H]], Boolean)]], acc: Chain[WithPos[Tidy[R]]]): Chain[WithPos[Tidy[R]]] = lists match {
          case Nil => acc
          case WithPos((list, true), pos) :: tail => {
            val results = ${ next.goAll(Cell(Push(CodeExpr('list)), tape), PosExpr('pos), '{ Chain.nil }) }
            loop(tail, acc ++ results)
          }
          case WithPos((list, false), pos) :: tail => {
            val all = ${ elem.goAll(Cell(Push(CodeExpr('list)), Empty), PosExpr('pos), '{ Chain.nil })(using Ctx.empty(ctx.s)) }
            loop(all.iterator ++: (WithPos((list, true), pos) :: tail), acc)
          }
        }

        loop(List(WithPos((Nil, false), ${ pos.toExpr })), $fail)
      }
    }
  }

  case class SaveProgress[T <: Tuple, R <: Tuple: Type](progress: Progress, next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goNext: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      goNext(tape, pos, fail)(using ctx.withProgress(progress, pos))
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }
  
    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goAll)
    }
  }

  case class RequireProgress[T <: Tuple, R <: Tuple: Type](progress: Progress, next: State[T, R]) extends State[T, R] {
    private def goWith[A: Type](tape: Tape[T], pos: Pos, fail: Expr[A], goNext: Go[T, A])(using ctx: Ctx[A])(using Quotes): Expr[A] = {
      val prev = ctx.progress(progress)
      '{
        if ${ pos.toExpr } > ${ prev.toExpr } then ${ goNext(tape, pos, fail) } else $fail
      }
    }

    override def go(tape: Tape[T], pos: Pos, fail: Expr[Option[Tidy[R]]])(using Ctx[Option[Tidy[R]]])(using Quotes): Expr[Option[Tidy[R]]] = {
      goWith(tape, pos, fail, next.go)
    }

    override def goPos(tape: Tape[T], pos: Pos, fail: Expr[Option[WithPos[Tidy[R]]]])(using Ctx[Option[WithPos[Tidy[R]]]])(using Quotes): Expr[Option[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goPos)
    }

    override def goAll(tape: Tape[T], pos: Pos, fail: Expr[Chain[WithPos[Tidy[R]]]])(using Ctx[Chain[WithPos[Tidy[R]]]])(using Quotes): Expr[Chain[WithPos[Tidy[R]]]] = {
      goWith(tape, pos, fail, next.goAll)
    }
  }

  extension [R <: Tuple: Type] (state: State[EmptyTuple, R]) {
    def run(s: Expr[String])(using Quotes): Expr[Option[Tidy[R]]] = state.go(Empty, PosInt(0), '{ None })(using Ctx.empty(s))
    def runAll(s: Expr[String])(using Quotes): Expr[List[Tidy[R]]] = '{ ${ state.goAll(Empty, PosInt(0), '{ Chain.nil })(using Ctx.empty(s)) }.toList.map(_.res) }
  }
}
