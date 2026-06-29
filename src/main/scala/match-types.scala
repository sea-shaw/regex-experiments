package experiments

import scala.compiletime.{summonInline}
import scala.compiletime.ops.int.{+}
import scala.compiletime.ops.string.{CharAt, Length}

object matchtypes {
  type Captures[S <: String & Singleton] = Tidy[Go[S, 0, EmptyTuple]]

  type Tidy[T <: Tuple] = T match {
    case EmptyTuple => Unit
    case Tuple1[a]  => a
    case _          => T
  }

  type Go[S <: String & Singleton, I <: Int & Singleton, Acc <: Tuple] <: Tuple = I match {
    case Length[S] => Acc
    case _         => CharAt[S, I] match {
      case '(' => Go[S, I + 1, String *: Acc]
      case _   => Go[S, I + 1, Acc]
    }
  }

  private val tests: Unit = {
    val zero: Captures["a"] = ()
    val one: Captures["(a)"] = "a"
    val two: Captures["(a)(b)"] = ("a", "b")
  }
}
