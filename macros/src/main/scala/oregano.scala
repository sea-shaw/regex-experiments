package experiments.macros

import experiments.macros.ast.AST
import experiments.macros.regex.{Regex, isInlineable}
import experiments.macros.utils.bimap
import scala.quoted.{Expr, Quotes, Type}

object oregano {
  type EitherIor[+A, +B] = Either[Either[A, B], (A, B)]

  private class Oregano(using Type[EitherIor]) extends AST {
    type InclusiveOr = EitherIor
    
    protected def fromLeft[A: Type](left: Expr[A])(using Quotes): Expr[InclusiveOr[A, Nothing]] = {
      '{ Left(Left($left)) }
    }

    protected def fromRight[B: Type](right: Expr[B])(using Quotes): Expr[InclusiveOr[Nothing, B]] = {
      '{ Left(Right($right)) }
    }

    protected def fromBoth[A: Type, B: Type](left: Expr[A], right: Expr[B])(using Quotes): Expr[InclusiveOr[A, B]] = {
      '{ Right(($left, $right)) }
    }

    override protected def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(expr: Expr[InclusiveOr[A, B]])(using Quotes): Expr[InclusiveOr[C, D]] = {
      '{
        val mapLeft = (left: A) => ${ f('left) }
        val mapRight = (right: B) => ${ g('right) }
        $expr.bimap(_.bimap(mapLeft, mapRight), _.bimap(mapLeft, mapRight))
      }
    }
  }

  extension (inline sc: StringContext) {
    transparent inline def r(): Regex[?] = ${ regexCode('sc) }
  }

  private def regexCode(sc: Expr[StringContext])(using Quotes): Expr[Regex[?]] = {
    isInlineable(sc, Oregano())
  }
}
