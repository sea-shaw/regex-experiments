package experiments.cps.macros

import experiments.cps.macros.pos.Pos
import scala.quoted.Expr

object context {
  class Ctx private (
    val s: Expr[String],
    private val starts: Map[Int, Pos],
  ) {
    def start(group: Int): Pos = starts(group)
    def withStart(group: Int, start: Pos): Ctx = Ctx(s, starts.updated(group, start))
  }

  object Ctx {
    def empty(s: Expr[String]): Ctx = Ctx(s, Map.empty)
  }
}
