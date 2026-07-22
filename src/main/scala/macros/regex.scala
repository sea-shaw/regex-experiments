package experiments.macros

import experiments.macros.ast.{Regex => RegexAST, Rep}
import experiments.macros.hcollections.hchain.{HChain, Tidy}
import experiments.macros.parser.Parser
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
      val parser = Parser()
      strExpr match {
        case Expr(s) => parser.parse(s) match {
          case Right(ast) => regexCode(s, ast)
          case Left(err)  => report.errorAndAbort(err, strExpr)
        }
        case _       => report.errorAndAbort("Regex string must be compile-time constant.", strExpr)
      }
    }

    private def regexCode[F[_ <: Rep] <: HChain](regexStr: String, ast: RegexAST[F])(using Quotes): Expr[Regex[Tidy[F[false]]]] = {      
      given Type[F] = ast.tpe

      val regexStrExpr = Expr(regexStr)
      '{
        new Regex[Tidy[F[false]]] {
          private val pattern: Pattern = Pattern.compile($regexStrExpr)

          override def unapply(s: String): Option[Tidy[F[false]]] = {
            val m = pattern.matcher(s)
            if (m.matches()) {
              val groups = Array.tabulate(m.groupCount) {i =>
                Option(m.group(i + 1))
              }
              val sanitised = ${ ast.sanitiseCode[false]('groups).runA(0).value }
              sanitised.value.map(_.captures.tidy)
            } else {
              None
            }
          }
        }
      }
    }
  }
}
