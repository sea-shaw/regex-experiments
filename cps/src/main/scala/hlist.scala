package experiments.cps

import experiments.cps.tidy.*

object hlist {
  sealed trait HList[T <: Tuple]
  case object HNil extends HList[EmptyTuple]
  case class HCons[H, T <: Tuple](head: H, tail: HList[T]) extends HList[H *: T]

  def tidy[T <: Tuple](hlist: HList[T]): Tidy[T] = hlist match {
    case HNil             => ()
    case HCons(x0, tail0) => tidyNonEmpty(x0, tail0)
  }

  def tidyNonEmpty[H, T <: Tuple](x0: H, tail0: HList[T]): TidyNonEmpty[H *: T] = tail0 match {
    case HNil             => x0
    case HCons(x1, tail1) => tail1 match {
      case HNil             => (x0, x1)
      case HCons(x2, tail2) => tail2 match {
        case HNil             => (x0, x1, x2)
        case HCons(x3, tail3) => tail3 match {
          case HNil => (x0, x1, x2, x3)
          case _    => ???
        }
      }
    }
  }
}
