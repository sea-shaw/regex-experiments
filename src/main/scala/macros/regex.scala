package experiments.macros

import experiments.macros.ast.{AST, Rep, Sanitised}
import experiments.macros.hcollections.hchain.HChain
import experiments.macros.parser
import java.util.regex.Pattern
import parsley.{Failure, Success}
import scala.quoted.{Expr, Quotes, quotes}
import scala.quoted.Type

object regex {
  sealed trait Regex[A] {
    def unapply(s: String): Option[A]
  }

  def isInlineable(sc: Expr[StringContext], ast: AST)(using Quotes): Expr[Regex[?]] = {
    import quotes.reflect.report
    sc match {
      case '{ StringContext(${ strExpr @ Expr(s) }) } => parser.parse(s, ast) match {
        case Success(regex) => regexCode(strExpr, ast)(regex)
        case Failure(err)  => report.errorAndAbort(err, strExpr)
      }
      case _       => report.errorAndAbort("Regex string must be compile-time constant.", sc)
    }
  }

  def code(exprStr: Expr[String], ast: AST)(using Quotes): Expr[String] = {
    exprStr match {
      case Expr(s) => parser.parse(s, ast) match {
        case Success(regex) => Expr(regexCode(exprStr, ast)(regex).show)
        case Failure(err)   => Expr(err)
      }
    }
  }

  private def regexCode[F[_ <: Rep] <: HChain](regexStr: Expr[String], ast: AST)(regex: ast.Regex[F])(using Quotes): Expr[Regex[?]] = {      
    given Type[F] = regex.tpe

    regex.tidyFunction[false] match {
      case tidy @ ast.TidyFunction(given Type[a]) => '{
        new Regex[a] {
          private val pattern: Pattern = Pattern.compile($regexStr)

          override def unapply(s: String): Option[a] = {
            val m = pattern.matcher(s)
            if (m.matches()) {
              val groups = Array.tabulate(m.groupCount) {i =>
                Option(m.group(i + 1))
              }
              val sanitised = ${ regex.sanitiseCode[false]('groups).runA(0).value }
              sanitised.value.map { case Sanitised(captures, _) =>
                ${ tidy('captures) }
              }
            } else {
              None
            }
          }
        }
      }
    }
  }
}
