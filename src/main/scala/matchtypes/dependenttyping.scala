package experiments.matchtypes

import experiments.matchtypes.matchtypes.{Captures, Fst, Go, Group, IsCapturing, Tidy}
import scala.compiletime.ops.int.+
import scala.compiletime.ops.string.{CharAt, Length}

object dependenttyping {

  def sanitiser[R <: String & Singleton](r: R): Array[String | Null] => Option[Captures[R]] = {
    go(r, 0, false, EmptyTuple).andThen(_.map(fst))
  }

  private def fst[T <: Tuple3[?, ?, ?]](t: T): Fst[T] = t match {
    case t: (_, _, _) => t._1
  }

  private def go[R <: String, I <: Int & Singleton, Cap <: Boolean & Singleton, Acc <: Tuple](r: R, i: I, cap: Cap, acc: Acc): Array[String | Null] => Option[Go[R, I, Cap, Acc]] = ???

  private def tidy[T <: Tuple](t: T): Tidy[T] = t match {
    case _: EmptyTuple => ()
    case t: Tuple1[_]  => t._1
    case t: Any        => t
  }

  private def isCapturing[R <: String, I <: Int](r: R, i: I): IsCapturing[R, I] = charAt(r, i) match {
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
    // TODO: Hide `+` from standard library
    private infix def plus[J <: Int & Singleton](j: J): I + J = (i + j).asInstanceOf[I + J]
  }

  private def charAt[S <: String, I <: Int](s: S, i: I): CharAt[S, I] = s.charAt(i).asInstanceOf[CharAt[S, I]]
}
