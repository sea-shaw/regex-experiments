package experiments.matchtypes

import experiments.matchtypes.matchtypes.Captures
import scala.quoted.{Expr, quotes, Quotes, Type}
import java.util.regex.Pattern

object macros {
  sealed trait Regex[+A] {
    def unapply(s: String): Option[A]
  }

  object Regex {
    transparent inline def apply[R <: String & Singleton](inline regex: R): Regex[Captures[R]] = ${ regexCode[Captures[R]]('regex) }
  }

  private def regexCode[A: Type](regex: Expr[String])(using Quotes): Expr[Regex[A]] = '{
    new Regex[A] {
      val pattern: Pattern = Pattern.compile($regex)
      override def unapply(s: String): Option[A] = {
        val m = pattern.matcher(s)
        if (m.matches()) {
          val a = Array.tabulate(m.groupCount)(i => m.group(i + 1))
          ${sanitiserCode[A]}(a, 0)._1
        } else {
          None
        }
      }
    }
  }

  private def sanitiserCode[A: Type](using Quotes): Expr[(Array[String | Null], Int) => (Option[A], Int, Boolean)] = {
    import quotes.reflect.report

    // TODO: I want to match on `Type.of[A]` but can't get the return type to work
    // I don't think this way of doing it will work for Option/Either/Tuple and anyway it's messy
    Expr.summon[Unit =:= A].map { ev =>
      '{ (groups: Array[String | Null], i: Int) => (Some($ev(())), i, false) }
    } orElse Expr.summon[String =:= A].map { ev =>
      '{ (groups: Array[String | Null], i: Int) =>
        val cap = Option(groups(i)).map($ev)
        (cap, i + 1, cap.isDefined)
      }
    } getOrElse {
      report.errorAndAbort("Unknown type")
    }
  }
}
