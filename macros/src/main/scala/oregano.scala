package experiments.macros

import cats.data.Ior
import cats.syntax.all.*
import experiments.macros.ast.AST
import experiments.macros.regex.{Regex, isInlineable}
import scala.quoted.{Expr, Quotes, Type}

object oregano {

  private object Oregano extends AST {
    type InclusiveOr[+A, +B] = Either[Either[A, B], (A, B)]

    override protected def inclusiveOrType(using Quotes): Type[InclusiveOr] = Type.of[InclusiveOr]

    override protected def fromOptions[A: Type, B: Type](using Quotes): Expr[(Option[A], Option[B]) => Option[Either[Either[A, B], (A, B)]]] = {
      '{ Ior.fromOptions(_, _).map(_.unwrap) }
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
    isInlineable(sc, Oregano)
  }
}
