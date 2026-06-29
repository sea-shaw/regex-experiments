package experiments

import scala.compiletime.ops.int.{+}
import scala.compiletime.ops.string.{CharAt, Length}
import scala.Tuple.{Concat, Reverse}

object matchtypes {

  type Captures[S <: String & Singleton] = Go[S, 0, Length[S], false, EmptyTuple] match {
    case (a, _) => a
  }

  type Go[S <: String, L <: Int, U <: Int, Cap <: Boolean, Acc <: Tuple] <: (Any, Int) = L match {
    case U => (Tidy[Reverse[Acc]], U)
    case _ => CharAt[S, L] match {
      case '\\' => Go[S, L + 2, Cap, U, Acc]
      case '('  => Go[S, L + 1, U, IsCapturing[S, L + 1], EmptyTuple] match {
        case (a, l) => a match {
          case Tuple => Go[S, l, U, Cap, Concat[a, Acc]]
          case Unit  => Go[S, l, U, Cap, Acc]
          case _     => Go[S, l, U, Cap, a *: Acc]
        }
      }
      case ')'  => L + 1 match {
        case U => (CloseGroup[Cap, Acc], U)
        case _ => CharAt[S, L + 1] match {
          case '?' | '*' => (Opt[CloseGroup[Cap, Acc]], L + 2)
          case _         => (CloseGroup[Cap, Acc], L + 1)
        }
      }
      case _    => Go[S, L + 1, U, Cap, Acc]
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
    val zero: Captures["a"] = ()
    val one: Captures["(a)"] = "a"
    val two: Captures["(a)(b)"] = ("a", "b")

    val escape: Captures["\\(a\\)"] = ()

    // TODO: Should these be flat or nested?
    val nested: Captures["(a(b))(c)"] = ("ab", "b", "c") // (("ab", "b"), "c")
    val manyNested: Captures["(a(b(c(d)))(e))"] = ("a", "b", "c", "d", "e") // ("a", ("b", ("c", "d")), "e")

    val optional: Captures["(a)?"] = Some("a")
    val catOptional: Captures["(a)?(b)?"] = (Some("a"), Some("b"))
    val catInOptional: Captures["(a(b))?"] = Some(("ab", "b"))
    val nestedOptional: Captures["(a(b)?)?"] = Some(("ab", Some("b")))

    val star: Captures["(a)*"] = Some("a")

    val nonCap: Captures["(?:a)"] = ()
    val capsInNonCap: Captures["(?:(a)(b))"] = ("a", "b")
    val capsInOptNonCap: Captures["(?:(a)(b))?"] = Some(("a", "b"))
    val optNonCap: Captures["(?:a)?"] = ()

    // TODO: Keep track of level of brackets
    val bug: Captures["(a))"] = "a"
  }
}
