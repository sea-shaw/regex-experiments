package experiments.macros

import experiments.macros.ast.Oregano
import experiments.macros.regex.{Regex, isInlineable}
import scala.quoted.{Expr, Quotes}

object oregano {
  extension (inline sc: StringContext) {
    transparent inline def r(): Regex[?] = ${ regexCode('sc) }
  }

  private def regexCode(sc: Expr[StringContext])(using Quotes): Expr[Regex[?]] = {
    isInlineable(sc, Oregano)
  }
}
