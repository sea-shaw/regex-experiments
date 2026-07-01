package experiments.matchtypes

import cats.syntax.all.catsSyntaxTuple2Semigroupal
import experiments.matchtypes.matchtypes.Captures
import java.util.regex.Pattern
import scala.quoted.{Expr, quotes, Quotes, Type}

object macros {

  type Sanitiser[A] = (Array[String | Null], Int) => (Option[A], Int, Boolean)

  sealed trait Regex[+A] {
    def unapply(s: String): Option[A]
  }

  object Regex {
    inline def apply[R <: String & Singleton](inline regex: R): Regex[Captures[R]] = ${ regexCode[Captures[R]]('regex) }
  }

  private def regexCode[A: Type](regex: Expr[String])(using Quotes): Expr[Regex[A]] = '{
    new Regex[A] {
      val pattern: Pattern = Pattern.compile($regex)
      override def unapply(s: String): Option[A] = {
        val m = pattern.matcher(s)
        if (m.matches()) {
          val a = Array.tabulate(m.groupCount)(i => m.group(i + 1))
          val sanitised = ${ sanitiseCode[A]('a, Expr(0)) }
          sanitised._1
        } else {
          None
        }
      }
    }
  }

  private def sanitiseCode[A: Type](groups: Expr[Array[String | Null]], i: Expr[Int])(using Quotes): Expr[(Option[A], Int, Boolean)] = {
    val sanitiser = Type.of[A] match {
      case '[Unit] => '{
        (Some(()), $i, false)
      }
      case '[EmptyTuple] => '{
        (Some(EmptyTuple), $i, false)
      }
      case '[String] => '{
        val cap = Option($groups($i))
        (cap, $i + 1, cap.isDefined)
      }
      case '[a *: as] => '{
        val (headCaps, j, anyHead) = ${ sanitiseCode[a](groups, i) }
        val (tailCaps, k, anyTail) = ${ sanitiseCode[as](groups, 'j) }
        ((headCaps, tailCaps).mapN(_ *: _), k, anyHead || anyTail)
      }
      case '[Option[a]] => '{
        val (caps, j, any) = ${ sanitiseCode[a](groups, i) }
        (Some(caps), j, any)
      }
      case '[Either[a, b]] => '{
        val (leftCaps, j, anyLeft) = ${ sanitiseCode[a](groups, i) }
        val (rightCaps, k, anyRight) = ${ sanitiseCode[b](groups, 'j) }
        val left = leftCaps.map(Left(_))
        val right = rightCaps.map(Right(_))
        val caps = if anyLeft then left.orElse(right) else right.orElse(left)
        (caps, k, anyLeft || anyRight)
      }
    }
    sanitiser.asExprOf[(Option[A], Int, Boolean)]
  }
}
