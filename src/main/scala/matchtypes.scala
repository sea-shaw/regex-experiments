package experiments

import cats.syntax.all.*
import java.util.regex.Pattern
import scala.compiletime.ops.int.{+, -}
import scala.compiletime.ops.string.{CharAt, Length}
import scala.Tuple.{Concat, Reverse}

object matchtypes {

  type Captures[S <: String & Singleton] = Go[S, 0, false, EmptyTuple] match {
    case (a, _, _) => a
  }

  type Go[S <: String, L <: Int, Cap <: Boolean, Acc <: Tuple] <: (Any, Int, Boolean) = L match {
    case Length[S] => (CloseGroup[Cap, Acc], Length[S], false)
    case _         => CharAt[S, L] match {
      case '\\' => Go[S, L + 2, Cap, Acc]
      case '|'  => Go[S, L + 1, false, EmptyTuple] match {
        case (Unit, l, opt) => Acc match {
          case EmptyTuple => (CloseGroup[Cap, EmptyTuple], l, opt)
          case _          => (CloseGroup[Cap, Tuple1[Either[Tidy[Reverse[Acc]], Unit]]], l, opt)
        }
        case (b, l, opt)    => (CloseGroup[Cap, Tuple1[Either[Tidy[Reverse[Acc]], b]]], l, opt)
      }
      case '('  => Go[S, L + 1, IsCapturing[S, L + 1], EmptyTuple] match {
        case (a, l, opt) => a match {
          case Unit  => Go[S, l, Cap, Acc]
          case _     => opt match {
            case true  => Go[S, l, Cap, Option[a] *: Acc]
            case false => a match {
              case Tuple => Go[S, l, Cap, Concat[a, Acc]]
              case _     => Go[S, l, Cap, a *: Acc]
            }
          }
        }
      }
      case ')'  => L + 1 match {
        case Length[S] => (CloseGroup[Cap, Acc], Length[S], false)
        case _         => CharAt[S, L + 1] match {
          case '?' | '*' => (CloseGroup[Cap, Acc], L + 2, true)
          case _         => (CloseGroup[Cap, Acc], L + 1, false)
        }
      }
      case _    => Go[S, L + 1, Cap, Acc]
    }
  }

  type Tidy[T <: Tuple] = T match {
    case EmptyTuple => Unit
    case Tuple1[a]  => a
    case _          => T
  }

  type IsCapturing[S <: String, I <: Int] <: Boolean = CharAt[S, I] match {
    case '?' => CharAt[S, I + 1] match {
      case '<' => CharAt[S, I + 2] match {
        case '=' | '!' => false
        case _         => true
      }
      case _ => false
    }
    case _ => true
  }

  type CloseGroup[Cap <: Boolean, Acc <: Tuple] = Cap match {
    case true  => Tidy[String *: Reverse[Acc]]
    case false => Tidy[Reverse[Acc]]
  }

  sealed trait Sanitiser[+T] {
    def sanitise(groups: Array[String | Null], i: Int): (Option[T], Int, Boolean)
  }

  object Sanitiser {
    given Sanitiser[Unit] {
      override def sanitise(captures: Array[String | Null], i: Int): (Some[Unit], i.type, false) = (Some(()), i, false)
    }

    given Sanitiser[EmptyTuple] {
      override def sanitise(groups: Array[String | Null], i: Int): (Some[EmptyTuple], i.type, false) = (Some(EmptyTuple), i, false)
    }

    given Sanitiser[String] {
      override def sanitise(groups: Array[String | Null], i: Int): (Option[String], Int, Boolean) = {
        val cap = Option(groups(i))
        (cap, i + 1, cap.isDefined)
      }
    }

    given [A, T <: Tuple](using head: Sanitiser[A], tail: Sanitiser[T]): Sanitiser[A *: T] = new {
      override def sanitise(groups: Array[String | Null], i: Int): (Option[A *: T], Int, Boolean) = {
        val (headCaps, j, anyHead) = head.sanitise(groups, i)
        val (tailCaps, k, anyTail) = tail.sanitise(groups, j)
        ((headCaps, tailCaps).mapN(_ *: _), k, anyHead || anyTail)
      }
    }

    given [A](using sanitiser: Sanitiser[A]): Sanitiser[Option[A]] = new {
      override def sanitise(groups: Array[String | Null], i: Int): (Some[Option[A]], Int, Boolean) = {
        val (caps, j, any) = sanitiser.sanitise(groups, i)
        (Some(caps), j, any)
      }
    }

    given [A, B](using leftSanitiser: Sanitiser[A], rightSanitiser: Sanitiser[B]): Sanitiser[Either[A, B]] = new {
      override def sanitise(groups: Array[String | Null], i: Int): (Option[Either[A, B]], Int, Boolean) = {
        val (leftCaps, j, anyLeft) = leftSanitiser.sanitise(groups, i)
        val (rightCaps, k, anyRight) = rightSanitiser.sanitise(groups, j)
        val left = leftCaps.map(Left(_))
        val right = rightCaps.map(Right(_))
        val caps = if anyLeft then left.orElse(right) else right.orElse(left)
        (caps, k, anyLeft || anyRight)
      }
    }
  }

  class Regex[A] private (regex: String)(using sanitiser: Sanitiser[A]) {
    val pattern: Pattern = Pattern.compile(regex)

    def unapply(s: String): Option[A] = {
      val m = pattern.matcher(s)
      if (m.matches()) {
        val a = Array.tabulate(m.groupCount)(i => m.group(i + 1))
        sanitiser.sanitise(a, 0)._1
      } else {
        None
      }
    }
  }

  object Regex {
    def apply[S <: String & Singleton](regex: S)(using Sanitiser[Captures[S]]): Regex[Captures[S]] = new Regex(regex)
  }

  private lazy val tests: Unit = {
    summon[Captures["a"] =:= Unit]
    summon[Captures["(a)"] =:= String]
    summon[Captures["(a)(b)"] =:= (String, String)]

    summon[Captures["\\(a\\)"] =:= Unit]

    // TODO: Should these be flat or nested?
    summon[Captures["(a(b))(c)"] =:= (String, String, String)] // ((String, String), String)
    summon[Captures["(a(b(c(d)))(e))"] =:= (String, String, String, String, String)] // (String, (String, (String, String)), String)

    summon[Captures["(a)?"] =:= Option[String]]
    summon[Captures["(a)?(b)?"] =:= (Option[String], Option[String])]
    summon[Captures["(a(b))?"] =:= Option[(String, String)]]
    summon[Captures["(a(b)?)?"] =:= Option[(String, Option[String])]]

    summon[Captures["(a)*"] =:= Option[String]]

    summon[Captures["(?:a)"] =:= Unit]
    summon[Captures["(?:a)(b)"] =:= String]
    summon[Captures["(?:(a)(b))"] =:= (String, String)]
    summon[Captures["(?:(a)(b))?"] =:= Option[(String, String)]]
    summon[Captures["(?:a)?"] =:= Unit]

    summon[Captures["a|b"] =:= Unit]
    summon[Captures["(a)|b"] =:= Either[String, Unit]]
    summon[Captures["(a)|(b)"] =:= Either[String, String]]
    summon[Captures["(a)|(b)|(c)"] =:= Either[String, Either[String, String]]]
    summon[Captures["(?:(a)|(b))|(?:(c))"] =:= Either[Either[String, String], String]]

    summon[Captures["(?:(a)|(b))?"] =:= Option[Either[String, String]]]
    summon[Captures["(?:(a)|(b)|(c))?"] =:= Option[Either[String, Either[String, String]]]]
    summon[Captures["(a)?|(b)?"] =:= Either[Option[String], Option[String]]]

    // TODO: Keep track of level of brackets
    summon[Captures["(a))"] =:= String] // Should not compile
    summon[Captures["(a"] =:= String] // Should not compile

    summon[Sanitiser[Captures["(?:(a)|(b)|(c))?"]]]
  }
}
