package experiments.matchtypes

import cats.syntax.all.catsSyntaxTuple2Semigroupal
import experiments.matchtypes.types.Captures
import java.util.regex.Pattern

object implicitsearch {
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
}
