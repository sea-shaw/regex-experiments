package experiments

import scala.compiletime.{summonInline}
import scala.compiletime.ops.int.{+}
import scala.compiletime.ops.string.{CharAt, Length}
import scala.Tuple.{Concat, Reverse}

object matchtypes {

  type Captures[S <: String & Singleton] = Go[S, 0, Length[S], EmptyTuple] match {
    case (a, _) => a
  }

  type Go[S <: String & Singleton, L <: Int, U <: Int, Acc <: Tuple] <: (Any, Int) = L match {
    case U => (Tidy[Reverse[Acc]], U)
    case _ => CharAt[S, L] match {
      case '\\' => Go[S, L + 2, U, Acc]
      case '('  => Go[S, L + 1, U, EmptyTuple] match {
        case (a, l) => a match {
          case Tuple => Go[S, l, U, Concat[a, Acc]]
          case _     => Go[S, l, U, a *: Acc]
        }
      }
      case ')'  => L + 1 match {
        case U => (Tidy[String *: Reverse[Acc]], U)
        case _ => CharAt[S, L + 1] match {
          case '?' => (Option[Tidy[String *: Reverse[Acc]]], L + 2)
          case _   => (Tidy[String *: Reverse[Acc]], L + 1)
        }
      }
      case _    => Go[S, L + 1, U, Acc]
    }
  }

  type Tidy[T <: Tuple] = T match {
    case EmptyTuple => Unit
    case Tuple1[a]  => a
    case _          => T
  }

  private val tests: Unit = {
    val zero: Captures["a"] = ()
    val one: Captures["(a)"] = "a"
    val two: Captures["(a)(b)"] = ("a", "b")

    val escape: Captures["\\(a\\)"] = ()

    // TODO: Should these be flat or nested?
    val nested: Captures["(a(b))(c)"] = ("abc", "b", "c")
    val manyNested: Captures["(a(b(c(d)))(e))"] = ("a", "b", "c", "d", "e")

    val optional: Captures["(a)?"] = Some("a")
    val catOptional: Captures["(a)?(b)?"] = (Some("a"), Some("b"))
    val nestedOptional: Captures["(a(b)?)?"] = Some(("ab", Some("b")))
  }
}
