package experiments.macros

import experiments.macros.ast.{Regex => RegexAST}
import experiments.macros.hlist.{HList, Tidy, tidy}
import experiments.macros.parser.parse
import scala.quoted.{Expr, Quotes, quotes}
import scala.quoted.Type
import java.util.regex.Pattern

object regex {
  sealed trait Regex[A] {
    def unapply(s: String): Option[A]
  }

  object Regex {
    transparent inline def apply(inline s: String): Regex[?] = ${ isInlineable('s) }

    private def isInlineable(s: Expr[String])(using Quotes): Expr[Regex[?]] = {
      import quotes.reflect.report
      s match {
        case Expr(s) => parse(s) match {
          case Right(ast) => regexCode(s, ast)
          case Left(err)  => report.errorAndAbort(err)
        }
        case _       => report.errorAndAbort("Regex string must be compile-time constant.")
      }
    }

    private def regexCode[A <: HList](regexStr: String, ast: RegexAST[A])(using Quotes): Expr[Regex[Tidy[A]]] = {
      import quotes.reflect.{Position, report}

      given Type[A] = ast.getType

      val regexStrExpr = Expr(regexStr)
      val expr = '{
        new Regex[Tidy[A]] {
          private val pattern: Pattern = Pattern.compile($regexStrExpr)

          override def unapply(s: String): Option[Tidy[A]] = {
            val m = pattern.matcher(s)
            if (m.matches()) {
              val groups = Array.tabulate(m.groupCount)(i => Option(m.group(i + 1)))
              val (sanitised, _) = ${ ast.sanitiseCode('groups, 0)._1 }
              sanitised.map(_.tidy)
            } else {
              None
            }
          }
        }
      }

      report.info(expr.show, Position.ofMacroExpansion)
      expr
    }
  }
}
