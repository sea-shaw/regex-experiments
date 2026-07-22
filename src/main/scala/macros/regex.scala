package experiments.macros

import experiments.macros.ast.{Regex => RegexAST, Rep}
import experiments.macros.hcollections.hchain.HChain
import experiments.macros.parser
import java.util.regex.Pattern
import parsley.{Failure, Success}
import scala.quoted.{Expr, Quotes, quotes}
import scala.quoted.Type
import experiments.macros.tidy.tidyFunction
import experiments.macros.tidy.TidyFunction

object regex {
  sealed trait Regex[A] {
    def unapply(s: String): Option[A]
  }

  object Regex {
    transparent inline def apply(inline s: String): Regex[?] = ${ isInlineable('s) }

    private def isInlineable(strExpr: Expr[String])(using Quotes): Expr[Regex[?]] = {
      import quotes.reflect.report
      strExpr match {
        case Expr(s) => parser.parse(s) match {
          case Success(ast) => regexCode(s, ast)
          case Failure(err)  => report.errorAndAbort(err, strExpr)
        }
        case _       => report.errorAndAbort("Regex string must be compile-time constant.", strExpr)
      }
    }

    private def regexCode[F[_ <: Rep] <: HChain](regexStr: String, ast: RegexAST[F])(using Quotes): Expr[Regex[?]] = {      
      given Type[F] = ast.tpe

      val tidy: TidyFunction[F[false], ?] = tidyFunction

      type A = tidy.tpe.Underlying
      given Type[A] = tidy.tpe

      val regexStrExpr = Expr(regexStr)
      '{
        new Regex[A] {
          private val pattern: Pattern = Pattern.compile($regexStrExpr)

          override def unapply(s: String): Option[A] = {
            val m = pattern.matcher(s)
            if (m.matches()) {
              val groups = Array.tabulate(m.groupCount) {i =>
                Option(m.group(i + 1))
              }
              val sanitised = ${ ast.sanitiseCode[false]('groups).runA(0).value }
              sanitised.value.map(s => ${tidy.tidy}(s.captures))
            } else {
              None
            }
          }
        }
      }
    }
  }
}
