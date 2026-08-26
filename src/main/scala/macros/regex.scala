package experiments.macros

import experiments.macros.ast.{AST, Rep}
import experiments.macros.hcollections.hchain.HChain
import experiments.macros.parsing.errors.{Pos, PosError, PosErrorBuilder}
import experiments.macros.parsing.parser.parse
import experiments.macros.sanitised.Sanitised
import java.util.regex.Pattern
import parsley.{Failure, Success}
import parsley.errors.ErrorBuilder
import scala.quoted.{Expr, Quotes, quotes}
import scala.quoted.Type

object regex {
  sealed trait Regex[A] {
    def unapply(s: String): Option[A]
  }

  def isInlineable(sc: Expr[StringContext], ast: AST)(using Quotes): Expr[Regex[?]] = {
    import quotes.reflect.report

    given ErrorBuilder[PosError] = PosErrorBuilder

    sc match {
      case '{ StringContext(${ strExpr @ Expr(s) }) } => parse(s, ast) match {
        case Success(regex)              => {
          // report.info(regex.toString, strExpr)
          regexCode(strExpr, ast)(regex)
        }
        case Failure(PosError(msg, pos)) => report.errorAndAbort(msg, errPos(strExpr, s, pos))
      }
      case _ => report.errorAndAbort("Regex string must be compile-time constant.", sc)
    }
  }

  private def errPos(expr: Expr[String], s: String, pos: Pos)(using q: Quotes): q.reflect.Position = {
    import quotes.reflect.{Position, asTerm}

    // TODO: Support multi-line regex?
    val (before, after) = s.splitAt(pos.offset)
    val exprPos = expr.asTerm.pos
    val start = exprPos.start + exprWidth(before)
    val end = start + exprWidth(after.take(pos.width))
    Position(exprPos.sourceFile, start, end)
  }

  private def exprWidth(s: String): Int = {
    s.foldLeft(0) {
      case (acc, '$' | '"') => acc + 2
      case (acc, _)         => acc + 1
    }
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
              val sanitised = ${ regex.sanitiseCode[false]('groups, 0) }
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
