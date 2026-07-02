package experiments.matchtypes

import experiments.matchtypes.matchtypes.{Captures, Fst, Go, IsCapturing, Tidy}
import scala.compiletime.ops.int.+
import scala.compiletime.ops.string.CharAt
import java.util.regex.Pattern
import scala.annotation.unused

object dependenttyping {

  class Regex[+A] private(regex: String, sanitise: Array[String | Null] => Option[A]) {
    private val pattern: Pattern = Pattern.compile(regex)
    def unapply(s: String): Option[A] = {
      val m = pattern.matcher(s)
      if (m.matches()) {
        val a = Array.tabulate(m.groupCount)(i => m.group(i + 1))
        sanitise(a)
      } else {
        None
      }
    }
  }

  object Regex {
    def apply[R <: String & Singleton](r: R): Regex[Captures[R]] = new Regex(r, sanitise(r))
  }

  def sanitise[R <: String & Singleton](r: R)(groups: Array[String | Null]): Option[Captures[R]] = {
    go(r, 0, false, EmptyTuple)(groups, 0)._1.map(fst)
  }

  private def fst[T <: Tuple3[?, ?, ?]](t: T): Fst[T] = t match {
    case t: (_, _, _) => t._1
  }

  // I think it's impossible to implement this without `asInstanceOf`, which defeats the point
  private def go[R <: String, I <: Int & Singleton, Cap <: Boolean & Singleton, Acc <: Tuple](r: R, i: I, cap: Cap, acc: Acc)(groups: Array[String | Null], groupNo: Int): (Option[Go[R, I, Cap, Acc]], Int, Boolean) = ???

  @unused private def tidy[T <: Tuple](t: T): Tidy[T] = t match {
    case _: EmptyTuple => ()
    case t: Tuple1[_]  => t._1
    case t: Any        => t
  }

  @unused private def isCapturing[R <: String, I <: Int](r: R, i: I): IsCapturing[R, I] = charAt(r, i) match {
    case _: '?' => charAt(r, i plus 1) match {
      case _: '<' => charAt(r, i plus 2) match {
        case _: ('=' | '!') => false
        case _: Any       => true
      }
      case _: Any => false
    }
    case _: Any => true
  }

  extension [I <: Int] (i: I) {
    private infix def plus[J <: Int & Singleton](j: J): I + J = (i + j).asInstanceOf[I + J]
  }

  private def charAt[S <: String, I <: Int](s: S, i: I): CharAt[S, I] = s.charAt(i).asInstanceOf[CharAt[S, I]]
}
