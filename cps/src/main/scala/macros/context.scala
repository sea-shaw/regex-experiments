package experiments.cps.macros

import scala.quoted.Expr

object context {
  final class JoinPoint[A]
  type Binding[_, _] = Nothing // TODO

  class Ctx[R] private (
    val s: Expr[String],
    private val bindings: Map[JoinPoint[?], Binding[?, R]],
    private val starts: Map[Int, Expr[Int]],
  ) {
    def binding[A](key: JoinPoint[A]): Binding[A, R] = bindings(key).asInstanceOf[Binding[A, R]]
    def withBinding[A](key: JoinPoint[A], value: Binding[A, R]): Ctx[R] = Ctx(s, bindings.updated(key, value), starts)
    def start(group: Int): Expr[Int] = starts(group)
    def withStart(group: Int, start: Expr[Int]): Ctx[R] = Ctx(s, bindings, starts.updated(group, start))
  }

  object Ctx {
    def empty[R](s: Expr[String]): Ctx[R] = Ctx(s, Map.empty, Map.empty)
  }
}
