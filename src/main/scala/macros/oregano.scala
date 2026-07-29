package experiments.macros

import experiments.macros.ast.Oregano
import experiments.macros.regex.{Regex, isInlineable}
import scala.quoted.{Expr, Quotes}

object oregano {
  object Regex {
    transparent inline def apply(inline s: String) = ${ regexCode('s) }
  }

  private def regexCode(s: Expr[String])(using Quotes): Expr[Regex[?]] = {
    isInlineable(s, Oregano)
  }
}
