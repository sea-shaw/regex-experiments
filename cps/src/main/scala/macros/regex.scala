package experiments.cps.macros

import experiments.cps.macros.ast.Reg
import experiments.cps.tidy.Tidy
import parsley.{Failure, Success}
import scala.quoted.{Expr, Quotes, Type, quotes}

object regex {
  sealed trait Regex[A] {
    def unapply(s: String): Option[A]
  }

  extension (sc: StringContext) {
    transparent inline def r(): Regex[?] = ${ isInlineable('sc) }
  }

  private def isInlineable(sc: Expr[StringContext])(using Quotes): Expr[Regex[?]] = {
    import quotes.reflect.{Position, report}

    sc match {
      case '{ StringContext(${ Expr(s) }) } => parser.parse(s) match {
        case Success(reg) => regexCode(reg)
        case Failure(msg) => report.errorAndAbort(msg, Position.ofMacroExpansion)
      }
    }
  }

  private def regexCode[A <: Tuple](reg: Reg[A])(using Quotes): Expr[Regex[Tidy[A]]] = {
    given Type[A] = reg.tpe
    val state = reg.compile

    '{
      new Regex[Tidy[A]] {
        override def unapply(s: String): Option[Tidy[A]] = ${ state.run('s) }
      }
    }
  }
}