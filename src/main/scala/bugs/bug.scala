package experiments.bugs

import scala.compiletime.ops.int.+

class C[A] {
  def unapply(s: String): Option[A] = ???
}

type MatchType[I <: Int, J <: Int] <: Tuple = I match {
  case J => EmptyTuple
  case _ => String *: MatchType[I + 1, J]
}

object C {
  def apply[I <: Int]: C[MatchType[0, I]] = new C
}

def f = {
  // Should not compile, but does
  {
    val x = C[2]
    "" match {
      case x("", "", "") => ???
    }
  }

  // Does not compile
  /* {
    val x: C[(String, String)] = C[2]
    "" match {
      case x("", "", "") => ???
    }
  } */

  // Does not compile
  /* {
    val x = C[2]
    "" match {
      case x("") => ???
    }
  } */

  // Does not compile
  /* {
    val x: C[(String, String, String)] = C[2]
    "" match {
      case x("", "", "") => ???
    }
  } */
}
