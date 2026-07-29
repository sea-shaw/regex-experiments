package experiments.macros

import experiments.macros.ast.Catnip
import experiments.macros.regex.{Regex, isInlineable}
import scala.quoted.{Expr, Quotes}

object catnip {
  extension (inline sc: StringContext) {
    transparent inline def r(): Regex[?] = ${ regexCode('sc) }
  }

  private def regexCode(sc: Expr[StringContext])(using Quotes): Expr[Regex[?]] = {
    isInlineable(sc, Catnip)
  }
}
