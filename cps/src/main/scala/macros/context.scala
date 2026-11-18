package experiments.cps.macros

import experiments.cps.macros.pos.Pos
import experiments.cps.macros.stack.Code
import experiments.cps.tidy.Tidy
import scala.quoted.{Expr, Quotes}

object context {
  type QJoin[A <: Tuple, R] = (Pos, Code[Tidy[A]], Expr[R]) => Quotes ?=> Expr[R]

  final class JoinPoint[A <: Tuple]
  private class Binding[A <: Tuple, R](val qjoin: QJoin[A, R])

  final class Progress

  case class Ctx[R] private (
    val s: Expr[String],
    private val bindings: Map[JoinPoint[?], Binding[?, R]],
    private val starts: Map[Int, Pos],
    private val progresses: Map[Progress, Pos],
  ) {
    def binding[A <: Tuple](key: JoinPoint[A]): QJoin[A, R] = bindings(key).qjoin.asInstanceOf
    def withBinding[A <: Tuple](key: JoinPoint[A], value: QJoin[A, R]): Ctx[R] = copy(bindings = bindings.updated(key, Binding(value)))
    def start(group: Int): Pos = starts(group)
    def withStart(group: Int, start: Pos): Ctx[R] = copy(starts = starts.updated(group, start))
    def progress(progress: Progress): Pos = progresses(progress)
    def withProgress(progress: Progress, pos: Pos) = copy(progresses = progresses.updated(progress, pos))
  }

  object Ctx {
    def empty[R](s: Expr[String]): Ctx[R] = Ctx(s, Map.empty, Map.empty, Map.empty)
    def nested[R](using ctx: Ctx[?]): Ctx[R] = empty(ctx.s)
  }
}
