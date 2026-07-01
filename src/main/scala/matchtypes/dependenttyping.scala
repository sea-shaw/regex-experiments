package experiments.matchtypes

import experiments.matchtypes.matchtypes.{Captures, Fst, Go, Group, IsCapturing, Tidy}
import scala.compiletime.ops.int.+
import scala.compiletime.ops.string.{CharAt, Length}
import scala.Tuple.{Concat}
import java.util.regex.Pattern

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
    go[R, 0, Length[R], false, EmptyTuple](r, 0, false, EmptyTuple)(groups, 0)._1.map(fst)
  }

  private def fst[T <: Tuple3[?, ?, ?]](t: T): Fst[T] = t match {
    case t: (_, _, _) => t._1
  }

  /** This just returns something of the correct type for `Go`.
    * Defaults to `""` for a capture group, `Some` for `?` and `Left` for `|`.
    */
  private def goShape[R <: String, I <: Int, U <: Int, Cap <: Boolean, Acc <: Tuple](r: R, i: I, cap: Cap, acc: Acc): Go[R, I, U, Cap, Acc] = i match {
    case u: U   => (groupShape(cap, acc), u, false) // TODO: Fix type test. Needs to be `U` or `Length[R]` but neither can be checked at runtime. Type test must be `=:=` to the one in the match type.
    case _: Any => charAt(r, i) match {
      case _: '\\' => goShape[R, I + 2, U, Cap, Acc](r, i plus 2, cap, acc)
      case _: '|'  => goShape[R, I + 1, U, false, EmptyTuple](r, i plus 1, false, EmptyTuple) match {
        case res: (_, _, _) => res._1 match {
          case _: Unit => acc match {
            case _: EmptyTuple => (groupShape(cap, EmptyTuple), res._2, res._3)
            case _: Any        => (groupShape(cap, Tuple1(Left(tidy(acc.reverse)))), res._2, res._3)         
          }
          case _: Any  => (groupShape(cap, Tuple1(Left(tidy(acc.reverse)))), res._2, res._3)
        }
      }
      case _: '('  => goShape[R, I + 1, U, IsCapturing[R, I + 1], EmptyTuple](r, i plus 1, isCapturing(r, i plus 1), EmptyTuple) match {
        case res: (a, l, opt) => res._1 match {
          case _: Unit => goShape[R, l, U, Cap, Acc](r, res._2, cap, acc)
          case _: Any  => res._3 match {
            case _: true  => goShape[R, l, U, Cap, Option[a] *: Acc](r, res._2, cap, Some(res._1) *: acc)
            case _: false => goShape[R, l, U, Cap, a *: Acc](r, res._2, cap, res._1 *: acc)
          }
        }
      }
      case _: ')'  => (i plus 1) match {
        case u: U   => (groupShape(cap, acc), u, false) // TODO: Fix type test.
        case _: Any => charAt(r, i plus 1) match {
          case _: ('?' | '*') => (groupShape(cap, acc), i plus 2, true)
          case _: Any         => (groupShape(cap, acc), i plus 1, false)
        }
      }
      case _: Any => goShape[R, I + 1, U, Cap, Acc](r, i plus 1, cap, acc)
    }
  }

  private def groupShape[Cap <: Boolean, Acc <: Tuple](cap: Cap, acc: Acc): Group[Cap, Acc] = cap match {
    case _: true => tidy("" *: acc.reverse)
    case _: false => tidy(acc.reverse)
  }

  // I think it's impossible to implement this without `asInstanceOf`, which defeats the point
  // Compiler can handle return type of `Go[...]` without `asInstanceOf` but not `Option[Go[...]]`
  private def go[R <: String, I <: Int & Singleton, U <: Int, Cap <: Boolean & Singleton, Acc <: Tuple](r: R, i: I, cap: Cap, acc: Acc)(groups: Array[String | Null], groupNo: Int): (Option[Go[R, I, U, Cap, Acc]], Int, Boolean) = ???

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

  // Neither of these functions exist in the standard library as far as I can
  // tell.

  extension [I <: Int] (i: I) {
    private infix def plus[J <: Int & Singleton](j: J): I + J = (i + j).asInstanceOf[I + J]
  }

  private def charAt[S <: String, I <: Int](s: S, i: I): CharAt[S, I] = s.charAt(i).asInstanceOf[CharAt[S, I]]
}
