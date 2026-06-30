package experiments

import cats.syntax.all.*
import java.util.regex.Pattern
import scala.compiletime.ops.int.{+, -}
import scala.compiletime.ops.string.{CharAt, Length}
import scala.Tuple.{Concat, Reverse}

object matchtypes {

  /**
    * Returns the type of the capture groups of the regular expression `R`.
    */
  type Captures[R <: String & Singleton] = Go[R, 0, false, EmptyTuple] match {
    case (a, _, _) => a
  }

  /**
    * Returns the type of the capture groups of the regular expression `R`
    * beginning at position `I` until the end of the regex or a `)` is reached
    * as well as the index of the next character in the regex and whether these
    * capture groups are optional.
    */
  type Go[R <: String, I <: Int, Cap <: Boolean, Acc <: Tuple] <: (Any, Int, Boolean) = I match {
    /* End of regex. */
    case Length[R] => (Group[Cap, Acc], Length[R], false)

    /* Check current character */
    case _         => CharAt[R, I] match {
      /* Escape character, so ignore next character. */
      case '\\' => Go[R, I + 2, Cap, Acc]

      /* Alternation. `Acc` is LHS, so get RHS and optionality using new empty
         accumulator and combine with `Either`, unless both contain no capture
         groups. */
      case '|'  => Go[R, I + 1, false, EmptyTuple] match {
        case (Unit, l, opt) => Acc match {
          case EmptyTuple => (Group[Cap, EmptyTuple], l, opt)
          case _          => (Group[Cap, Tuple1[Either[Tidy[Reverse[Acc]], Unit]]], l, opt)
        }
        case (b, l, opt)    => (Group[Cap, Tuple1[Either[Tidy[Reverse[Acc]], b]]], l, opt)
      }

      /* Beginning of group. Get type of group, and add it to `Acc`. Continue
         from next character after group. */
      case '('  => Go[R, I + 1, IsCapturing[R, I + 1], EmptyTuple] match {
        case (a, l, opt) => a match {
          case Unit  => Go[R, l, Cap, Acc]
          case _     => opt match {
            case true  => Go[R, l, Cap, Option[a] *: Acc]
            case false => a match {
              case Tuple => Go[R, l, Cap, Concat[a, Acc]]
              case _     => Go[R, l, Cap, a *: Acc]
            }
          }
        }
      }

      /* End of group, so return type of group and optionality. */
      case ')'  => I + 1 match {
        case Length[R] => (Group[Cap, Acc], Length[R], false)
        case _         => CharAt[R, I + 1] match {
          case '?' | '*' => (Group[Cap, Acc], I + 2, true)
          case _         => (Group[Cap, Acc], I + 1, false)
        }
      }

      case _    => Go[R, I + 1, Cap, Acc]
    }
  }

  /**
    * Returns `Unit` for empty tuples, the element type for singleton tuples, or
    * the tuple if it has two or more elements. Makes it easier to pattern match
    * as there is no syntactic sugar for empty or singleton tuples.
    */
  type Tidy[T <: Tuple] = T match {
    case EmptyTuple => Unit
    case Tuple1[a]  => a
    case _          => T
  }

  /**
    * Returns whether the group begining at position `I` in the regular
    * expression `R` is capturing. Note `I` is the position of the character
    * after the `(` at the begining of the group.
    */
  type IsCapturing[R <: String, I <: Int] <: Boolean = CharAt[R, I] match {
    case '?' => CharAt[R, I + 1] match {
      case '<' => CharAt[R, I + 2] match {
        case '=' | '!' => false
        case _         => true
      }
      case _ => false
    }
    case _ => true
  }

  /**
    * Returns the type of a group and its sub-groups. `Cap` is whether or not
    * the top-level group is capturing. `Acc` is the accumulator built by `Go`
    * for this group.
    */
  type Group[Cap <: Boolean, Acc <: Tuple] = Cap match {
    case true  => Tidy[String *: Reverse[Acc]]
    case false => Tidy[Reverse[Acc]]
  }

  sealed trait Sanitiser[+A] {
    /**
      * Returns sanitised capture groups from `groups` of type `A`, the index of
      * the next capture group in `groups`, and whether the capture groups of
      * type `A` included any non-null matches.
      */
    def sanitise(groups: Array[String | Null], i: Int): (Option[A], Int, Boolean)
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

  class Regex[+A] private (regex: String)(using sanitiser: Sanitiser[A]) {
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
    def apply[R <: String & Singleton](regex: R)(using Sanitiser[Captures[R]]): Regex[Captures[R]] = new Regex(regex)
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
