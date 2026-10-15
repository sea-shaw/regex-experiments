package experiments.cps.macros

import scala.quoted.{Expr, Quotes, quotes}

object utils {
  inline def code[A](inline x: A): String = ${ codeCode('x) }

  private def codeCode[A](expr: Expr[A])(using Quotes): Expr[String] = {
    import quotes.reflect.{Printer, asTerm}
    Expr(expr.asTerm.show(using Printer.TreeShortCode))
  }
}
