package experiments.macros

import scala.quoted.{Expr, Quotes, Type}

object macroextractor {

  object Alt {
    inline def unapply[A, B](inline alt: Either[A, B]): Tuple1[A] = ${ unapplyCode('alt) }
  }

  private def unapplyCode[A: Type, B: Type](expr: Expr[Either[A, B]])(using Quotes): Expr[Tuple1[A]] = {
    val extracted = extractCode(expr)
    '{ Tuple1($extracted) }
  }

  private def extractCode[A: Type, B: Type](expr: Expr[Either[A, B]])(using Quotes): Expr[A] = Type.of[B] match {
    case '[A] => '{
      $expr match {
        case Left(a) => a
        case Right(b) => b
      }
    }.asExprOf[A]
    case '[Either[A, b]] => '{ 
      $expr match {
        case Left(a) => a
        case Right(b) => ${ extractCode[A, b]('b.asExprOf[Either[A, b]]) }
      }
    }
  }
}
