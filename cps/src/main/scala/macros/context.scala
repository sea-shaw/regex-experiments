package experiments.cps.macros

import experiments.cps.macros.pos.Pos
import experiments.cps.macros.stack.Code
import experiments.cps.tidy.Tidy
import scala.quoted.{Expr, Quotes}

object context {
  final class JoinPoint[A <: Tuple]
  case class Binding[A <: Tuple, R <: Tuple](qjoin: (Pos, Code[Tidy[A]]) => Quotes ?=> Expr[Option[Tidy[R]]])

  class Ctx[R <: Tuple] private (
    val s: Expr[String],
    private val bindings: Map[JoinPoint[?], Binding[?, R]],
    private val starts: Map[Int, Pos],
  ) {
    def binding[A <: Tuple](key: JoinPoint[A]): Binding[A, R] = bindings(key).asInstanceOf[Binding[A, R]]
    def withBinding[A <: Tuple](key: JoinPoint[A], value: Binding[A, R]): Ctx[R] = Ctx(s, bindings.updated(key, value), starts)
    def start(group: Int): Pos = starts(group)
    def withStart(group: Int, start: Pos): Ctx[R] = Ctx(s, bindings, starts.updated(group, start))
  }

  object Ctx {
    def empty[R <: Tuple](s: Expr[String]): Ctx[R] = Ctx(s, Map.empty, Map.empty)
  }
}
