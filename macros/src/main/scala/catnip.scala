package experiments.macros

import cats.data.Ior
import experiments.macros.ast.AST
import experiments.macros.regex.{Regex, isInlineable}
import scala.quoted.{Expr, Quotes, Type}

object catnip {

  private object Catnip extends AST {
    type InclusiveOr = Ior

    override protected def inclusiveOrType(using Quotes): Type[InclusiveOr] = Type.of[InclusiveOr]

    override protected def fromOptions[A: Type, B: Type](using Quotes): Expr[(Option[A], Option[B]) => Option[InclusiveOr[A, B]]] = {
      '{ Ior.fromOptions }
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

  inline def code(inline s: String) = ${ codeCode('s) }

  private def codeCode(s: Expr[String])(using Quotes): Expr[String] = regex.code(s, Catnip)
}
