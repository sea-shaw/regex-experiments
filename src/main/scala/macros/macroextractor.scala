package experiments.macros

import scala.quoted.{Expr, Quotes, Type, quotes}

object macroextractor {

  object Alt {
    inline def unapply[A, B](inline alt: Either[A, B]): (Int, A) = ${ unapplyCode('alt) }
  }

  private def unapplyCode[A: Type, B: Type](expr: Expr[Either[A, B]])(using Quotes): Expr[(Int, A)] = {
    extractCode(0, expr)
  }

  private def extractCode[A: Type, B: Type](idx: Int, expr: Expr[Either[A, B]])(using Quotes): Expr[(Int, A)] = {
    import quotes.reflect.{Position, report}

    // TODO: Can we do this without both `case '[A]` and `Expr.summon[B <:< A]`.
    // I still think this is better than `asExprOf`.
    // Need `case '[Either[A, b]]` to deconstruct types and get access to `b`.
    // Need `Expr.summon` to avoid `asExprOf` 
    val extracted = Type.of[B] match {
      case '[A] => Expr.summon[B <:< A].map { ev =>
        val i = Expr(idx)
        val j = Expr(idx + 1)
        '{
          $expr match {
            case Left(a) => ($i, a)
            case Right(b) => ($j, $ev(b))
          }
        }
      }
      case '[Either[A, b]] => Expr.summon[B <:< Either[A, b]].map { ev =>
        val i = Expr(idx)
        '{
          $expr match {
            case Left(a) => ($i, a)
            case Right(b) => {
              val right = $ev(b)
              ${ extractCode[A, b](idx + 1, 'right) }
            }
          }
        }
      }
      case _ => None
    }

    extracted.getOrElse(report.errorAndAbort(s"Invalid type", Position.ofMacroExpansion))
  }
}
