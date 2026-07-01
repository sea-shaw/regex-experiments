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
          ${sanitiserCode[A]}(a, 0)._1
        } else {
          None
        }
      }
    }
  }

  private def sanitiserCode[A: Type](using Quotes): Expr[Sanitiser[A]] = {
    val sanitiser = Type.of[A] match {
      case '[Unit] => '{ (_: Array[String | Null], i: Int) =>
        (Some(()), i, false)
      }
      case '[EmptyTuple] => '{ (_: Array[String | Null], i: Int) =>
        (Some(EmptyTuple), i, false)
      }
      case '[String] => '{ (groups: Array[String | Null], i: Int) =>
        val cap = Option(groups(i))
        (cap, i, cap.isDefined)
      }
      case '[a *: as] => '{ (groups: Array[String | Null], i: Int) =>
        val (headCaps, j, anyHead) = ${sanitiserCode[a]}(groups, i)
        val (tailCaps, k, anyTail) = ${sanitiserCode[as]}(groups, j)
        ((headCaps, tailCaps).mapN(_ *: _), k, anyHead || anyTail)
      }
      case '[Option[a]] => '{ (groups: Array[String | Null], i: Int) =>
        val (caps, j, any) = ${sanitiserCode[a]}(groups, i)
        (Some(caps), j, any)
      }
      case '[Either[a, b]] => '{ (groups: Array[String | Null], i: Int) =>
        val (leftCaps, j, anyLeft) = ${sanitiserCode[a]}(groups, i)
        val (rightCaps, k, anyRight) = ${sanitiserCode[b]}(groups, j)
        val left = leftCaps.map(Left(_))
        val right = rightCaps.map(Right(_))
        val caps = if anyLeft then left.orElse(right) else right.orElse(left)
        (caps, k, anyLeft || anyRight)
      }
    }
    sanitiser.asExprOf[Sanitiser[A]]
  }
}
