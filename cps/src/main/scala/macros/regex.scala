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

  extension (inline sc: StringContext) {
    transparent inline def r(): Regex[?] = ${ rCode('sc) }
    transparent inline def r1(): Regex[?] = ${ r1Code('sc) }
  }

  private def rCode(sc: Expr[StringContext])(using Quotes): Expr[Regex[?]] = {
    given Config = Config(ignoreEmpty = false)
    isInlineable(sc)
  }

  private def r1Code(sc: Expr[StringContext])(using Quotes): Expr[Regex[?]] = {
    given Config = Config(ignoreEmpty = true)
    isInlineable(sc)
  }

  private def isInlineable(sc: Expr[StringContext])(using Config)(using Quotes): Expr[Regex[?]] = {
    import quotes.reflect.{Position, report}

    sc match {
      case '{ StringContext(${ Expr(s) }) } => parser.parse(s) match {
        case Success(reg) => regexCode(reg)
        case Failure(msg) => report.errorAndAbort(msg, Position.ofMacroExpansion)
      }
      case _ => report.errorAndAbort("Regular expression must be a compile-time constant", Position.ofMacroExpansion)
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
