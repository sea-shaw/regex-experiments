package experiments.macros

import scala.quoted.{Expr, Quotes, Type, quotes}

object macroextractor {

  object Alt {
    inline def unapply[A, B](inline alt: Either[A, B]): (Int, A) = ${ unapplyCode('alt) }
  }

  private def unapplyCode[A: Type, B: Type](expr: Expr[Either[A, B]])(using Quotes): Expr[(Int, A)] = {
    import quotes.reflect.{Position, report}

    val extracted = extractCode(0, expr)
    report.info(extracted.show, Position.ofMacroExpansion)
    extracted
  }

  private def extractCode[A: Type, B: Type](idx: Int, expr: Expr[Either[A, B]])(using Quotes): Expr[(Int, A)] = {
    import quotes.reflect.{Position, TypeRepr, report}
    
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

    extracted.getOrElse(report.errorAndAbort(s"Invalid type: ${TypeRepr.of[Either[A, B]].show}", Position.ofMacroExpansion))
  }
}
