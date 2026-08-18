package experiments.macros

import cats.data.Ior
import experiments.macros.tidy.Tidy
import experiments.macros.regex.{Regex, isInlineable}
import scala.quoted.{Expr, Quotes, Type}

object catnip {

  private object Catnip extends Tidy {
    type InclusiveOr = Ior

    override protected def inclusiveOrType(using Quotes): Type[InclusiveOr] = Type.of[InclusiveOr]

    override protected def fromOptions[A: Type, B: Type](left: Expr[Option[A]], right: Expr[Option[B]])(using Quotes): Expr[Option[InclusiveOr[A, B]]] = {
      '{ Ior.fromOptions($left, $right) }
    }

    override protected def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(expr: Expr[Ior[A, B]])(using Quotes): Expr[InclusiveOr[C, D]] = {
      '{ $expr.bimap(left => ${ f('left) }, right => ${ g('right) }) }
    }
  }

  extension (inline sc: StringContext) {
    transparent inline def r(): Regex[?] = ${ regexCode('sc) }
  }

  private def regexCode(sc: Expr[StringContext])(using Quotes): Expr[Regex[?]] = {
    isInlineable(sc, Catnip)
  }
}
