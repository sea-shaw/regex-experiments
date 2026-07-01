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

  private def regexCode[A: Type](regex: Expr[String])(using Quotes): Expr[Regex[A]] = '{
    new Regex[A] {
      val pattern: Pattern = Pattern.compile($regex)
      override def unapply(s: String): Option[A] = {
        val m = pattern.matcher(s)
        if (m.matches()) {
          val a = Array.tabulate(m.groupCount)(i => m.group(i + 1))
          val (sanitised, _, _) = ${ sanitiseCode[A]('a, Expr(0)) }
          sanitised
        } else {
          None
        }
      }
    }
  }

  private def sanitiseCode[A: Type](groups: Expr[Array[String | Null]], i: Expr[Int])(using Quotes): Expr[(Option[A], Int, Boolean)] = {
    val sanitised = Type.of[A] match {
      case '[Unit]         => '{ (Some(()), $i, false) }
      case '[EmptyTuple]   => '{ (Some(EmptyTuple), $i, false) }
      case '[String]       => sanitiseStringCode(groups, i)
      case '[a *: t]       => sanitiseTupleCode[a, t](groups, i)
      case '[Option[a]]    => sanitiseOptionCode[a](groups, i)
      case '[Either[a, b]] => sanitiseEitherCode[a, b](groups, i)
    }
    sanitised.asExprOf[(Option[A], Int, Boolean)]
  }

  private def sanitiseStringCode(groups: Expr[Array[String | Null]], i: Expr[Int])(using Quotes): Expr[(Option[String], Int, Boolean)] = '{
    val cap = Option($groups($i))
    (cap, $i + 1, cap.isDefined)
  }

  private def sanitiseTupleCode[A: Type, T <: Tuple: Type](groups: Expr[Array[String | Null]], i: Expr[Int])(using Quotes): Expr[(Option[A *: T], Int, Boolean)] = '{
    val (headCaps, j, anyHead) = ${ sanitiseCode[A](groups, i) }
    val (tailCaps, k, anyTail) = ${ sanitiseCode[T](groups, 'j) }
    ((headCaps, tailCaps).mapN(_ *: _), k, anyHead || anyTail)
  }

  private def sanitiseOptionCode[A: Type](groups: Expr[Array[String | Null]], i: Expr[Int])(using Quotes): Expr[(Option[Option[A]], Int, Boolean)] = '{
    val (caps, j, any) = ${ sanitiseCode[A](groups, i) }
    (Some(caps), j, any)
  }

  private def sanitiseEitherCode[A: Type, B: Type](groups: Expr[Array[String | Null]], i: Expr[Int])(using Quotes): Expr[(Option[Either[A, B]], Int, Boolean)] = '{
    val (leftCaps, j, anyLeft) = ${ sanitiseCode[A](groups, i) }
    val (rightCaps, k, anyRight) = ${ sanitiseCode[B](groups, 'j) }
    val left = leftCaps.map(Left(_))
    val right = rightCaps.map(Right(_))
    val caps = if anyLeft then left.orElse(right) else right.orElse(left)
    (caps, k, anyLeft || anyRight)
  }
}
