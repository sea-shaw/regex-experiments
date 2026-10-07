package experiments.cps.macros

import scala.quoted.{Expr, Quotes}

object pos {
  sealed trait Pos {
    def toExpr(using Quotes): Expr[Int]
    def +(m: Int): Pos
  }
  case class PosInt(n: Int) extends Pos {
    override def toExpr(using Quotes): Expr[Int] = Expr(n)
    override def +(m: Int): Pos = PosInt(n + m)
  }
  case class PosExpr(expr: Expr[Int]) extends Pos {
    override def toExpr(using Quotes): Expr[Int] = expr
    override def +(m: Int): Pos = PosBoth(expr, m)
  }
  case class PosBoth(expr: Expr[Int], n: Int) extends Pos {
    override def toExpr(using Quotes): Expr[Int] = '{ $expr + ${ Expr(n) } }
    override def +(m: Int): Pos = PosBoth(expr, n + m)
  }
}
