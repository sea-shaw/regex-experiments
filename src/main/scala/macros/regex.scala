package experiments.macros

import experiments.macros.ast.{Regex => RegexAST}
import experiments.macros.hcollections.hchain.{HChain, Tidy}
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

    private def isInlineable(strExpr: Expr[String])(using Quotes): Expr[Regex[?]] = {
      import quotes.reflect.report
      strExpr match {
        case Expr(s) => parse(s) match {
          case Right(ast) => regexCode(s, ast)
          case Left(err)  => report.errorAndAbort(err, strExpr)
        }
        case _       => report.errorAndAbort("Regex string must be compile-time constant.", strExpr)
      }
    }

    private def regexCode[A <: HChain](regexStr: String, ast: RegexAST[A])(using Quotes): Expr[Regex[Tidy[A]]] = {      
      given Type[A] = ast.getType

      val regexStrExpr = Expr(regexStr)
      '{
        new Regex[Tidy[A]] {
          private val pattern: Pattern = Pattern.compile($regexStrExpr)

          override def unapply(s: String): Option[Tidy[A]] = {
            val m = pattern.matcher(s)
            if (m.matches()) {
              val groups = Array.tabulate(m.groupCount) {i =>
                Option(m.group(i + 1)).map((_, m.end(i + 1)))
              }
              val sanitised = ${ ast.sanitiseCode('groups, 0).sanitised }
              sanitised.map(_.captures.tidy)
            } else {
              None
            }
          }
        }
      }
    }
  }
}
