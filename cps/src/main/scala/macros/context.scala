package experiments.cps.macros

import experiments.cps.macros.pos.Pos
import experiments.cps.macros.stack.Code
import scala.quoted.{Expr, Quotes}

object context {
  final class JoinPoint[A]
  case class Binding[A, R](qjoin: (Expr[Int], Code[A]) => Quotes ?=> Code[Option[R]])

  class Ctx[R] private (
    val s: Expr[String],
    private val bindings: Map[JoinPoint[?], Binding[?, R]],
    private val starts: Map[Int, Pos],
  ) {
    def binding[A](key: JoinPoint[A]): Binding[A, R] = bindings(key).asInstanceOf[Binding[A, R]]
    def withBinding[A](key: JoinPoint[A], value: Binding[A, R]): Ctx[R] = Ctx(s, bindings.updated(key, value), starts)
    def start(group: Int): Pos = starts(group)
    def withStart(group: Int, start: Pos): Ctx[R] = Ctx(s, bindings, starts.updated(group, start))
  }

  object Ctx {
    def empty[R](s: Expr[String]): Ctx[R] = Ctx(s, Map.empty, Map.empty)
  }
}
