package experiments.macros

import experiments.macros.ast.{AST, Rep, RepFalse}
import experiments.macros.hchain.HChain
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
        case Success(regex)              => regexCode(strExpr, ast)(regex)
        case Failure(PosError(msg, pos)) => report.errorAndAbort(msg, errPos(strExpr, s, pos))
      }
      case _ => report.errorAndAbort("Regex string must be compile-time constant.", sc)
    }
  }

  /* Calculates the position of the error in the string expression. `expr` is
     the actual expression, `s` is the contenst of the expression, and `pos` is
     the error position reported by the parser. */
  private def errPos(expr: Expr[String], s: String, pos: Pos)(using q: Quotes): q.reflect.Position = {
    import quotes.reflect.{Position, asTerm}

    // TODO: Support multi-line regex?
    /* Parser errors are in terms of row, column, and width. Scala errors use
       character offset from the start of the file. Need to convert between
       them. */
    val (before, after) = s.splitAt(pos.offset)
    val exprPos = expr.asTerm.pos
    val start = exprPos.start + exprWidth(before)
    val end = start + exprWidth(after.take(pos.width))
    Position(exprPos.sourceFile, start, end)
  }

  /* Calculates the width of the expression representing `s`. */
  // TODO: Only works for the string-interpolated version `r"..."`. Calling
  //       `StringContext("...").r()` explicitly will result in an incorrect
  //       position, but who would do that?
  private def exprWidth(s: String): Int = {
    s.foldLeft(0) {
      case (acc, '$' | '"') => acc + 2 /* `$$` and `$"` in the interpolated string, so 2 characters wide. */
      case (acc, _)         => acc + 1
    }
  }

  /* Returns a string containing a representation of the inlined code generated
     for the regex `exprStr`. */
  def code(exprStr: Expr[String], ast: AST)(using Quotes): Expr[String] = {
    exprStr match {
      case Expr(s) => parse(s, ast) match {
        case Success(regex) => {
          import quotes.reflect.{Printer, asTerm}
          val codeExpr = regexCode(exprStr, ast)(regex)
          val codeStr = codeExpr.asTerm.show(using Printer.TreeShortCode)

          /* `_`s used in lambdas and type lambdas are given numeric identifiers
             which can change even if the rest is unchanged, so remove these
             identifiers to prevent changes in the output. */
          Expr(codeStr.replaceAll("_\\$\\d+", "_\\$"))
        }
        case Failure(err)   => Expr(err)
      }
    }
  }

  /* Returns the inlined code for a `Regex` */
  private def regexCode[F[_ <: Rep] <: HChain](regexStr: Expr[String], ast: AST)(regex: ast.Regex[F])(using Quotes): Expr[Regex[?]] = {
    given Type[F] = regex.nodeType.tpe

    regex.nodeType match {
      /* Don't generate an array or `HChain` for a regex with 0 capturing groups. */
      case _: ast.HEmptyType => '{
        new Regex[Unit] {
          private val pattern: Pattern = Pattern.compile($regexStr)
          override def unapply(s: String): Option[Unit] = {
            Option.when(pattern.matcher(s).matches())(())
          }
        }
      }
      case _: ast.HNonEmptyType[_] => regex.tidyFunction(using RepFalse) match {
        case tidy @ ast.TidyFunction(given Type[a]) => '{
          new Regex[a] {
            private val pattern: Pattern = Pattern.compile($regexStr)

            override def unapply(s: String): Option[a] = {
              val m = pattern.matcher(s)
              if (m.matches()) {
                val groups = Array.tabulate(m.groupCount) {i =>
                  Option(m.group(i + 1))
                }
                val sanitised = ${ regex.sanitiseCode('groups, 0)(using RepFalse) }
                sanitised.value.map { case Sanitised(node, _) =>
                  ${ tidy('node) }
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
}
