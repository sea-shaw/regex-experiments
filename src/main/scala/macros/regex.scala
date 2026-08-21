package experiments.macros

import experiments.macros.ast.{AST, Rep, Sanitised}
import experiments.macros.hcollections.hchain.HChain
import experiments.macros.parsing.parser
import java.util.regex.Pattern
import parsley.{Failure, Success}
import scala.quoted.{Expr, Quotes, quotes}
import scala.quoted.Type
import experiments.macros.parsing.errors.PosErrorBuilder
import parsley.errors.ErrorBuilder
import experiments.macros.parsing.errors.PosError
import experiments.macros.parsing.errors.Pos

object regex {
  sealed trait Regex[A] {
    def unapply(s: String): Option[A]
  }

  def isInlineable(sc: Expr[StringContext], ast: AST)(using Quotes): Expr[Regex[?]] = {
    import quotes.reflect.{asTerm, report}
    given ErrorBuilder[PosError] = PosErrorBuilder
    sc match {
      case '{ StringContext(${ strExpr @ Expr(s) }) } => parser.parse(s, ast) match {
        case Success(regex) => regexCode(strExpr, ast)(regex)
        case Failure(err)  => report.errorAndAbort(err.msg, pos(strExpr.asTerm.pos, err.pos.offset, err.pos.width))
      }
      case _       => report.errorAndAbort("Regex string must be compile-time constant.", sc)
    }
  }

  private def pos(using q: Quotes)(pos: q.reflect.Position, offset: Int, width: Int): q.reflect.Position = {
    import quotes.reflect.Position
    val start = pos.start + offset
    val end = start + width
    Position(pos.sourceFile, start, end)
  }

  def code(exprStr: Expr[String], ast: AST)(using Quotes): Expr[String] = {
    exprStr match {
      case Expr(s) => parser.parse(s, ast) match {
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
