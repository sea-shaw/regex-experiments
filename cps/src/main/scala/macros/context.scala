package experiments.cps.macros

import experiments.cps.macros.pos.Pos
import experiments.cps.macros.stack.Code
import experiments.cps.tidy.Tidy
import scala.quoted.{Expr, Quotes}

object context {
  final class JoinPoint[A <: Tuple]
  type QJoin[A <: Tuple, R <: Tuple] = (Pos, Code[Tidy[A]]) => Quotes ?=> Expr[Option[Tidy[R]]]
  private case class Binding[A <: Tuple, R <: Tuple](qjoin: QJoin[A, R])

  class Ctx[R <: Tuple] private (
    val s: Expr[String],
    private val bindings: Map[JoinPoint[?], Binding[?, R]],
    private val starts: Map[Int, Pos],
  ) {
    def binding[A <: Tuple](key: JoinPoint[A]): QJoin[A, R] = bindings(key).qjoin.asInstanceOf
    def withBinding[A <: Tuple](key: JoinPoint[A], value: QJoin[A, R]): Ctx[R] = Ctx(s, bindings.updated(key, Binding(value)), starts)
    def start(group: Int): Pos = starts(group)
    def withStart(group: Int, start: Pos): Ctx[R] = Ctx(s, bindings, starts.updated(group, start))
  }

  object Ctx {
    def empty[R <: Tuple](s: Expr[String]): Ctx[R] = Ctx(s, Map.empty, Map.empty)
  }
}
