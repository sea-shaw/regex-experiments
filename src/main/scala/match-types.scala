package experiments

import scala.compiletime.ops.int.{+, -}
import scala.compiletime.ops.string.{CharAt, Length}
import scala.Tuple.{Concat, Reverse}

object matchtypes {

  type Captures[S <: String & Singleton] = Fst[Go[S, 0, false, EmptyTuple]]

  type Fst[T <: Tuple2[?, ?]] = T match {
    case (a, _) => a
  }

  type Go[S <: String, L <: Int, Cap <: Boolean, Acc <: Tuple] <: (Any, Int) = L match {
    case Length[S] => (Tidy[Reverse[Acc]], Length[S])
    case _ => CharAt[S, L] match {
      case '\\' => Go[S, L + 2, Cap, Acc]
      case '|'  => Go[S, L + 1, false, EmptyTuple] match {
        case (Unit, l) => Acc match {
          case EmptyTuple => (CloseGroup[Cap, EmptyTuple], l)
          case _    => (CloseGroup[Cap, Tuple1[Either[Tidy[Reverse[Acc]], Unit]]], l)
        }
        case (b, l)    => (CloseGroup[Cap, Tuple1[Either[Tidy[Reverse[Acc]], b]]], l)
      }
      case '('  => Go[S, L + 1, IsCapturing[S, L + 1], EmptyTuple] match {
        case (a, l) => a match {
          case Tuple => Go[S, l, Cap, Concat[a, Acc]]
          case Unit  => Go[S, l, Cap, Acc]
          case _     => Go[S, l, Cap, a *: Acc]
        }
      }
      case ')'  => L + 1 match {
        case Length[S] => (CloseGroup[Cap, Acc], Length[S])
        case _ => CharAt[S, L + 1] match {
          case '?' | '*' => (Opt[CloseGroup[Cap, Acc]], L + 2)
          case _         => (CloseGroup[Cap, Acc], L + 1)
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

  type Opt[A] = A match {
    case Unit => Unit
    case _    => Option[A]
  }

  type Alt[A, B] = (A, B) match {
    case (Unit, Unit) => Unit
    case _            => Either[A, B]
  }

  private val tests: Unit = {
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

    /* Bugs, should not compile */
    // TODO: Keep track of level of brackets
    summon[Captures["(a))"] =:= String]
    summon[Captures["(a"] =:= Unit]

    summon[Captures["a|b"] =:= Unit]
    summon[Captures["(a)|b"] =:= Either[String, Unit]]
    summon[Captures["(a)|(b)"] =:= Either[String, String]]
    summon[Captures["(a)|(b)|(c)"] =:= Either[String, Either[String, String]]]
    summon[Captures["(?:(a)|(b))|(?:(c))"] =:= Either[Either[String, String], String]]
  }
}
