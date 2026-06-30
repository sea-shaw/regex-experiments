package experiments

import cats.syntax.all.*
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

  sealed trait Extractor[+T] {
    def extract(groups: Array[String | Null], i: Int): (Option[T], Int)
  }

  object Extractor {
    given Extractor[Unit] {
      override def extract(captures: Array[String | Null], i: Int): (Some[Unit], i.type) = (Some(()), i)
    }

    given Extractor[EmptyTuple] {
      override def extract(groups: Array[String | Null], i: Int): (Some[EmptyTuple], i.type) = (Some(EmptyTuple), i)
    }

    given Extractor[String] {
      override def extract(groups: Array[String | Null], i: Int): (Option[String], Int) = (Option(groups(i)), i + 1)
    }

    given [A, T <: Tuple](using head: Extractor[A], tail: Extractor[T]): Extractor[A *: T] = new {
      override def extract(groups: Array[String | Null], i: Int): (Option[A *: T], Int) = {
        val (headCaps, j) = head.extract(groups, i)
        val (tailCaps, k) = tail.extract(groups, j)
        ((headCaps, tailCaps).mapN(_ *: _), k)
      }
    }

    given [A](using extractor: Extractor[A]): Extractor[Option[A]] = new {
      override def extract(groups: Array[String | Null], i: Int): (Some[Option[A]], Int) = {
        val (caps, j) = extractor.extract(groups, i)
        (Some(caps), j)
      }
    }

    given [A, B](using leftExtractor: Extractor[A], rightExtractor: Extractor[B]): Extractor[Either[A, B]] = new {
      override def extract(groups: Array[String | Null], i: Int): (Option[Either[A, B]], Int) = {
        val (leftCaps, j) = leftExtractor.extract(groups, i)
        val (rightCaps, k) = rightExtractor.extract(groups, j)
        val anyLeft = i != j
        val left = leftCaps.map(Left(_))
        val right = rightCaps.map(Right(_))
        val caps = if anyLeft then left.orElse(right) else right.orElse(left)
        (caps, k)
      }
    }
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

    summon[Extractor[Captures["(?:(a)|(b)|(c))?"]]]
  }
}
