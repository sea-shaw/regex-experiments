package experiments.cps.macros

import experiments.cps.macros.ast.Reg
import experiments.cps.macros.config.Config
import experiments.cps.tidy.Tidy
import parsley.{Failure, Success}
import scala.quoted.{Expr, Quotes, Type, quotes}

object regex {
  sealed trait Regex[A] {
    def unapply(s: String): Option[A]
    // def all(s: String): List[A]
  }

  extension (inline sc: StringContext)(using inline config: Config) {
    transparent inline def r(): Regex[?] = ${ isInlineable('sc, 'config) }
  }

  private def isInlineable(sc: Expr[StringContext], config: Expr[Config])(using Quotes): Expr[Regex[?]] = {
    import quotes.reflect.{Printer, asTerm, report}

    sc match {
      case '{ StringContext(${ Expr(s) }) } => config match {
        case Expr(given Config) => parser.parse(s) match {
          case Success(reg) => regexCode(reg)
          case Failure(msg) => report.errorAndAbort(msg)
        }
        case _ => report.errorAndAbort(s"Config must be a compile-time constant but was ${ config.asTerm.show(using Printer.TreeShortCode) }")
      }
      case _ => report.errorAndAbort("Regular expression must be a compile-time constant")
    }
  }

  private def regexCode[A <: Tuple](reg: Reg[A])(using Quotes): Expr[Regex[Tidy[A]]] = {
    given Type[A] = reg.tpe
    val state = reg.compile

    '{
      new Regex[Tidy[A]] {
        override def unapply(s: String): Option[Tidy[A]] = ${ state.run('s) }
        // override def all(s: String): List[Tidy[A]] = ${ state.runAll('s) }
      }
    }
  }
}
