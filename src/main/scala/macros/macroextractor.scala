package experiments.macros

import scala.quoted.{Expr, Quotes, Type, quotes}

object macroextractor {

  object Alt {
    inline def unapply[A, B](inline alt: Either[A, B]): Tuple1[A] = ${ unapplyCode('alt) }
  }

  private def unapplyCode[A: Type, B: Type](expr: Expr[Either[A, B]])(using Quotes): Expr[Tuple1[A]] = {
    import quotes.reflect.{Position, report}

    val extracted = extractCode(expr)
    report.info(extracted.show, Position.ofMacroExpansion)
    extracted
  }

  private def extractCode[A: Type, B: Type](expr: Expr[Either[A, B]])(using Quotes): Expr[Tuple1[A]] = Type.of[B] match {
    case '[A] => '{ $expr.fold(Tuple1(_), Tuple1(_)) }.asExprOf[Tuple1[A]] 
    case '[Either[A, b]] => '{ $expr.fold(Tuple1(_), b => ${ extractCode('b.asExprOf[Either[A, b]]) }) }
  }
}
