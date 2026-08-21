package experiments.macros

import experiments.macros.ast.{AST, Rep, Sanitised}
import experiments.macros.hcollections.hchain.HChain
import experiments.macros.parsing.errors.{Pos, PosError, PosErrorBuilder}
import experiments.macros.parsing.parser.parse
import java.util.regex.Pattern
import parsley.{Failure, Success}
import scala.quoted.{Expr, Quotes, quotes}
import scala.quoted.Type
import parsley.errors.ErrorBuilder

object regex {
  sealed trait Regex[A] {
    def unapply(s: String): Option[A]
  }

  def isInlineable(sc: Expr[StringContext], ast: AST)(using Quotes): Expr[Regex[?]] = {
    import quotes.reflect.report

    given ErrorBuilder[PosError] = PosErrorBuilder

    sc match {
      case '{ StringContext(${ strExpr @ Expr(s) }) } => parse(s, ast) match {
        case Success(regex) => regexCode(strExpr, ast)(regex)
        case Failure(PosError(msg, pos))   => report.errorAndAbort(msg, errPos(strExpr, pos))
      }
      case _ => report.errorAndAbort("Regex string must be compile-time constant.", sc)
    }
  }

  private def errPos(expr: Expr[?], pos: Pos)(using q: Quotes): q.reflect.Position = {
    import quotes.reflect.{Position, asTerm}

    val exprPos = expr.asTerm.pos
    val start = exprPos.start + pos.offset
    val end = start + pos.width
    Position(exprPos.sourceFile, start, end)
  }

  def code(exprStr: Expr[String], ast: AST)(using Quotes): Expr[String] = {
    exprStr match {
      case Expr(s) => parse(s, ast) match {
        case Success(regex) => {
          import quotes.reflect.{Printer, asTerm}
          val codeExpr = regexCode(exprStr, ast)(regex)
          val codeStr = codeExpr.asTerm.show(using Printer.TreeShortCode)
          Expr(codeStr)
        }
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
