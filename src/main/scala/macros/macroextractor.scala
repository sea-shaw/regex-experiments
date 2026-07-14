package experiments.macros

import scala.quoted.{Expr, Quotes, Type}

object macroextractor {

  object Alt {
    inline def unapply[A, B](inline alt: Either[A, B]): (Int, A) = ${ unapplyCode('alt) }
  }

  private def unapplyCode[A: Type, B: Type](expr: Expr[Either[A, B]])(using Quotes): Expr[(Int, A)] = {
    extractCode(0, expr)
  }

  private def extractCode[A: Type, B: Type](idx: Int, altExpr: Expr[Either[A, B]])(using Quotes): Expr[(Int, A)] = {
    // import quotes.reflect.{Position, report}

    val extract = Type.of[B] match {
      case '[A] => {
        val i = Expr(idx)
        val j = Expr(idx + 1)
        val expr = '{
          $altExpr match {
            case Left(a) => ($i, a)
            case Right(b) => ($j, b)
          }
        }
        expr.asExprOf[(Int, A)]
      }
      case '[Either[A, b]] => {
        val i = Expr(idx)
        '{
          $altExpr match {
            case Left(a) => ($i, a)
            case Right(b) => ${ extractCode[A, b](idx + 1, 'b.asExprOf[Either[A, b]]) }
          }
        }
      }
    }
    // report.info(extract.show, Position.ofMacroExpansion)
    extract
  }
}
