package experiments.matchtypes

import cats.syntax.all.catsSyntaxTuple2Semigroupal
import experiments.matchtypes.matchtypes.Captures
import java.util.regex.Pattern
import scala.quoted.{Expr, quotes, Quotes, Type}

object macros {

  sealed trait Regex[+A] {
    def unapply(s: String): Option[A]
  }

  object Regex {
    inline def apply[R <: String & Singleton](inline regex: R): Regex[Captures[R]] = ${ regexCode[Captures[R]]('regex) }
  }

  private def regexCode[A: Type](regex: Expr[String])(using Quotes): Expr[Regex[A]] = {
    import quotes.reflect.{Position, report}
    val expr = '{
      new Regex[A] {
        val pattern: Pattern = Pattern.compile($regex)
        override def unapply(s: String): Option[A] = {
          val m = pattern.matcher(s)
          if (m.matches()) {
            val a = Array.tabulate(m.groupCount)(i => m.group(i + 1))
            val (sanitised, _) = ${ sanitiseCode[A]('a, 0)._1 }
            sanitised
          } else {
            None
          }
        }
      }
    }
    report.info(expr.show, Position.ofMacroExpansion)
    expr
  }

  private def sanitiseCode[A: Type](groups: Expr[Array[String | Null]], i: Int)(using Quotes): (Expr[(Option[A], Boolean)], Int) = {
    val (sanitised, j) = Type.of[A] match {
      case '[Unit]         => ('{ (Some(()), false) }, i)
      case '[EmptyTuple]   => ('{ (Some(EmptyTuple), false) }, i)
      case '[String]       => sanitiseStringCode(groups, i)
      case '[a *: t]       => sanitiseTupleCode[a, t](groups, i)
      case '[Option[a]]    => sanitiseOptionCode[a](groups, i)
      case '[Either[a, b]] => sanitiseEitherCode[a, b](groups, i)
    }
    (sanitised.asExprOf[(Option[A], Boolean)], j)
  }

  private def sanitiseStringCode(groups: Expr[Array[String | Null]], i: Int)(using Quotes): (Expr[(Option[String], Boolean)], Int) = {
    val idx = Expr(i)
    val sanitised = '{
      val cap = Option($groups($idx))
      (cap, cap.isDefined)
    }
    (sanitised, i + 1)
  }

  private def sanitiseTupleCode[A: Type, T <: Tuple: Type](groups: Expr[Array[String | Null]], i: Int)(using Quotes): (Expr[(Option[A *: T], Boolean)], Int) = {
    val (sanitisedHead, j) = sanitiseCode[A](groups, i)
    val (sanitisedTail, k) = sanitiseCode[T](groups, j)
    val sanitised = '{
      val (headCaps, anyHead) = $sanitisedHead
      val (tailCaps, anyTail) = $sanitisedTail
      ((headCaps, tailCaps).mapN(_ *: _), anyHead || anyTail)
    }
    (sanitised, k)
  }

  private def sanitiseOptionCode[A: Type](groups: Expr[Array[String | Null]], i: Int)(using Quotes): (Expr[(Option[Option[A]], Boolean)], Int) = {
    val (sanitisedValue, j) = sanitiseCode[A](groups, i)
    val sanitised = '{
      val (caps, any) = $sanitisedValue
      (Some(caps), any)
    }
    (sanitised, j)
  }

  private def sanitiseEitherCode[A: Type, B: Type](groups: Expr[Array[String | Null]], i: Int)(using Quotes): (Expr[(Option[Either[A, B]], Boolean)], Int) = {
    val (sanitisedLeft, j) = sanitiseCode[A](groups, i)
    val (sanitisedRight, k) = sanitiseCode[B](groups, j)
    val sanitised = '{
      val (leftCaps, anyLeft) = $sanitisedLeft
      val (rightCaps, anyRight) = $sanitisedRight
      val left = leftCaps.map(Left(_))
      val right = rightCaps.map(Right(_))
      val caps = if anyLeft then left.orElse(right) else right.orElse(left)
      (caps, anyLeft || anyRight)
    }
    (sanitised, k)
  }
}
