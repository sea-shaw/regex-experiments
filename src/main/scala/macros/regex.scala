package experiments.macros

import experiments.macros.ast.{Regex => RegexAST, Rep}
import experiments.macros.hcollections.hchain.{HChain, Tidy}
import experiments.macros.evidence.apply
import experiments.macros.parser.parse
import scala.quoted.{Expr, Quotes, quotes}
import scala.quoted.Type
import java.util.regex.Pattern

object regex {

  type Unapply[A] = A match {
    case Unit => Boolean
    case _    => Option[A]
  }

  sealed trait Regex[A] {
    def unapply(s: String): Unapply[A]
  }

  object Regex {
    transparent inline def apply(inline s: String): Regex[?] = ${ isInlineable('s) }

    private def isInlineable(strExpr: Expr[String])(using Quotes): Expr[Regex[?]] = {
      import quotes.reflect.report
      strExpr match {
        case Expr(s) => parse(s) match {
          case Right(ast) => regexCode(s, ast)
          case Left(err)  => report.errorAndAbort(err, strExpr)
        }
        case _       => report.errorAndAbort("Regex string must be compile-time constant.", strExpr)
      }
    }

    private def regexCode[F[_ <: Rep] <: HChain](regexStr: String, ast: RegexAST[F])(using Quotes): Expr[Regex[Tidy[F[false]]]] = {      
      given Type[F] = ast.getType

      val regexStrExpr = Expr(regexStr)
      '{
        new Regex[Tidy[F[false]]] {
          private val pattern: Pattern = Pattern.compile($regexStrExpr)

          override def unapply(s: String): Unapply[Tidy[F[false]]] = ${ unapplyCode[F](ast, 'pattern, 's) }
        }
      }
    }

    private def unapplyCode[F[_ <: Rep] <: HChain: Type](ast: RegexAST[F], pattern: Expr[Pattern], s: Expr[String])(using Quotes): Expr[Unapply[Tidy[F[false]]]] = {
      type A = Tidy[F[false]]

      val expr = Expr.summon[Boolean =:= Unapply[A]].map { ev =>
        ev {
            '{
            val m = $pattern.matcher($s)
            m.matches
          }
        }
      } orElse Expr.summon[Option[A] =:= Unapply[A]].map { ev =>
        ev {
          '{
            val m = $pattern.matcher($s)
            if (m.matches()) {
              val groups = Array.tabulate(m.groupCount) {i =>
                Option(m.group(i + 1))
              }
              val sanitised = ${ ast.sanitiseCode[false]('groups, 0).sanitised }
              sanitised.map(_.captures.tidy)
            } else {
              None
            }
          }
        }
      }

      expr.get
    }
  }
}
