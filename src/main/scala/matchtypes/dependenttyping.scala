package experiments.matchtypes

import experiments.matchtypes.matchtypes.{Captures, Fst, Go, Group, IsCapturing, OptionCaptures, OptionGo, OptionGroup, Tidy}
import scala.compiletime.ops.any.==
import scala.compiletime.ops.int.+
import scala.compiletime.ops.string.{CharAt, Length}
import scala.Tuple.{Concat => ++}
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
    def apply[R <: String & Singleton](r: R): Regex[OptionCaptures[R]] = new Regex(r, ???)
  }

  private def optionCaptures[R <: String & Singleton](r: R)(groups: Array[String | Null]): OptionCaptures[R] = optionGo[R, 0, Length[R], false, EmptyTuple](r, 0, length(r), false, EmptyTuple)(groups, 0, false) match {
    case t: ((_, _, _), _, _) => t._1._1
  }

  // I think it's impossible to implement this without `asInstanceOf`, which defeats the point
  // Compiler can handle return type of `Go[...]` without `asInstanceOf` but not `Option[Go[...]]`
  private def optionGo[R <: String, I <: Int, U <: Length[R], Cap <: Boolean, Acc <: Tuple](r: R, i: I, u: U, cap: Cap, acc: Acc)(groups: Array[String | Null], groupNo: Int, any: Boolean): OptionGo[R, I, U, Cap, Acc] = equals(i, u) match {
    case _: true  => ((optionGroup(cap, acc)(groups, groupNo), u, false), groupNo, any)
    case _: false => charAt(r, i) match {
      case _: '\\' => optionGo(r, plus(i, 2), u, cap, acc)(groups, groupNo, any)
      case _: '|'  => optionGo(r, plus(i, 1), u, false, EmptyTuple)(groups, groupNo, false) match {
        // Why isn't this giving a type test warning? SBT and Metals both don't complain.
        case t: ((Option[b], l, opt), Int, Boolean) => ???
      }
      case _: '('  => ???
      case _: ')'  => equals(plus(i, 1), u) match {
        case _: true => {
          val capture = optionGroup(cap, acc)(groups, groupNo)
          ((capture, u, false), groupNo + 1, capture.isDefined)
        }
        case _: false => charAt(r, plus(i, 1)) match {
          case _: ('?' | '*') => {
            val capture = optionGroup(cap, acc)(groups, groupNo)
            ((capture, plus(i, 2), true), groupNo + 1, capture.isDefined)
          }
          case _: Any         => {
            val capture = optionGroup(cap, acc)(groups, groupNo)
            ((capture, plus(i, 1), false), groupNo + 1, capture.isDefined)
          }
        }
      }
      case _: Any  => optionGo(r, plus(i, 1), u, cap, acc)(groups, groupNo, any)
    }
  }

  private def optionGroup[Cap <: Boolean, Acc <: Tuple](cap: Cap, acc: Acc)(groups: Array[String | Null], groupNo: Int): OptionGroup[Cap, Acc] = cap match {
    case _: true => Option(groups(groupNo)).map(s => tidy(s *: acc.reverse))
    case _: false => Some(tidy(acc.reverse))
  }

  def capturesShape[R <: String & Singleton](r: R): Captures[R] = fst(goShape(r, 0, length(r), false, EmptyTuple))

  private def fst[T <: Tuple3[?, ?, ?]](t: T): Fst[T] = t match {
    case t: (_, _, _) => t._1
  }

  /** This just returns something of the correct type for `Go`.
    * Defaults to `""` for a capture group, `Some` for `?` and `Left` for `|`.
    */
  private def goShape[R <: String, I <: Int, U <: Length[R], Cap <: Boolean, Acc <: Tuple](r: R, i: I, u: U, cap: Cap, acc: Acc): Go[R, I, U, Cap, Acc] = (equals(i, u)) match {
    case _: true  => (groupShape(cap, acc), u, false)
    case _: false => charAt(r, i) match {
      case _: '\\' => goShape[R, I + 2, U, Cap, Acc](r, plus(i, 2), u, cap, acc)
      case _: '|'  => goShape[R, I + 1, U, false, EmptyTuple](r, plus(i, 1), u, false, EmptyTuple) match {
        case res: (_, _, _) => res._1 match {
          case _: Unit => acc match {
            case _: EmptyTuple => (groupShape(cap, EmptyTuple), res._2, res._3)
            case _: Any        => (groupShape(cap, Tuple1(Left(tidy(acc.reverse)))), res._2, res._3)         
          }
          case _: Any  => (groupShape(cap, Tuple1(Left(tidy(acc.reverse)))), res._2, res._3)
        }
      }
      case _: '('  => goShape[R, I + 1, U, IsCapturing[R, I + 1], EmptyTuple](r, plus(i, 1), u, isCapturing(r, plus(i, 1)), EmptyTuple) match {
        case res: (a, l, _) => res._1 match {
          case _: Unit  => goShape[R, l, U, Cap, Acc](r, res._2, u, cap, acc)
          case t: Tuple => res._3 match {
            case _: true  => goShape[R, l, U, Cap, Option[a] *: Acc](r, res._2, u, cap, Some(res._1) *: acc)
            case _: false => goShape[R, l, U, Cap, (a & Tuple) ++ Acc](r, res._2, u, cap, (t ++ acc))
          }
          case _: Any   => res._3 match {
            case _: true  => goShape[R, l, U, Cap, Option[a] *: Acc](r, res._2, u, cap, Some(res._1) *: acc)
            case _: false => goShape[R, l, U, Cap, a *: Acc](r, res._2, u, cap, res._1 *: acc)
          }
        }
      }
      case _: ')'  => equals(plus(i, 1), u) match {
        case _: true  => (groupShape(cap, acc), u, false)
        case _: false => charAt(r, plus(i, 1)) match {
          case _: ('?' | '*') => (groupShape(cap, acc), plus(i, 2), true)
          case _: Any         => (groupShape(cap, acc), plus(i, 1), false)
        }
      }
      case _: Any  => goShape[R, I + 1, U, Cap, Acc](r, plus(i, 1), u, cap, acc)
    }
  }

  private def groupShape[Cap <: Boolean, Acc <: Tuple](cap: Cap, acc: Acc): Group[Cap, Acc] = cap match {
    case _: true => tidy("" *: acc.reverse)
    case _: false => tidy(acc.reverse)
  }

  private def tidy[T <: Tuple](t: T): Tidy[T] = t match {
    case _: EmptyTuple => ()
    case t: Tuple1[_]  => t._1
    case t: Any        => t
  }

  private def isCapturing[R <: String, I <: Int](r: R, i: I): IsCapturing[R, I] = charAt(r, i) match {
    case _: '?' => charAt(r, plus(i, 1)) match {
      case _: '<' => charAt(r, plus(i, 2)) match {
        case _: ('=' | '!') => false
        case _: Any       => true
      }
      case _: Any => false
    }
    case _: Any => true
  }

  // None of these functions exist in the standard library as far as I can tell.
  private def plus[I <: Int, J <: Int & Singleton](i: I, j: J) = (i + j).asInstanceOf[I + J]
  private def equals[X, Y](x: X, y: Y): X == Y = (x == y).asInstanceOf[X == Y]
  private def length[S <: String](s: S): Length[S] = s.length.asInstanceOf[Length[S]]
  private def charAt[S <: String, I <: Int](s: S, i: I): CharAt[S, I] = s.charAt(i).asInstanceOf[CharAt[S, I]]
}
