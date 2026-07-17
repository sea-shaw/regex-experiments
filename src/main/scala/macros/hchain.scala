package experiments.macros

import scala.annotation.tailrec

object hchain {

  type HConcat[A <: HChain, B <: HChain] = A match {
    case HEmpty    => B
    case HNonEmpty => B match {
      case HEmpty    => A
      case HNonEmpty => HAppend[A & HNonEmpty, B & HNonEmpty]
    }
  }

  type HPrepended[A, B <: HChain] = B match {
    case HEmpty    => HSingleton[A]
    case HNonEmpty => HAppend[HSingleton[A], B & HNonEmpty]
  }

  type Tidy[A <: HChain] = TidyTuple[ToTuple[A]]

  type TidyTuple[T <: Tuple] = T match {
    case EmptyTuple => Unit
    case Tuple1[a]  => a
    case _          => T
  }

  type ToTuple[A <: HChain] = Build[A, EmptyTuple]

  // TODO: Remove recusrive match type.
  type Build[A <: HChain, Acc <: Tuple] <: Tuple = A match {
    case HEmpty => Acc
    case HSingleton[a] => a *: Acc
    case HAppend[a, b] => Build[a & HNonEmpty, Build[b & HNonEmpty, Acc]]
  }

  sealed trait HChain {
    def +:[A, B >: this.type <: HChain](x: A): HPrepended[A, B] = (this: B) match {
      case _: HEmpty             => Singleton(x)
      case nonEmptyXs: HNonEmpty => Append(Singleton(x), nonEmptyXs)
    }
  }

  sealed trait HEmpty extends HChain
  private case object Empty extends HEmpty

  sealed trait HNonEmpty extends HChain

  sealed trait HSingleton[+A] extends HNonEmpty
  private case class Singleton[+A](x: A) extends HSingleton[A]

  sealed trait HAppend[+A, +B] extends HNonEmpty
  private case class Append[+A <: HNonEmpty, +B <: HNonEmpty](left: A, right: B) extends HAppend[A, B]

  extension [A <: HChain] (xs: A) {
    def ++[B <: HChain](ys: B): HConcat[A, B] = xs match {
      case _: HEmpty             => ys
      case nonEmptyXs: HNonEmpty => ys match {
        case _: HEmpty             => nonEmptyXs
        case nonEmptyYs: HNonEmpty => Append(nonEmptyXs, nonEmptyYs)
      }
    }

    // TODO: `ArrayBuilder`? Don't want to use `*:` because it's O(n^2)
    def tidy: Tidy[A] = {
      @tailrec
      def toList(xs: HChain, acc: List[Any], rest: List[HNonEmpty]): List[Any] = xs match {
        case Empty => rest match {
          case Nil => acc
          case head :: tail => toList(head, acc, tail)
        }
        case Singleton(x) => toList(Empty, x :: acc, rest)
        case Append(left, right) => toList(right, acc, left :: rest)
      }

      val tup = Tuple.fromArray(toList(xs, Nil, Nil).toArray).asInstanceOf[ToTuple[A]]

      tup match {
        case _: EmptyTuple  => ()
        case tup: Tuple1[_] => tup._1
        case _: Any         => tup
      }
    }
  }

  object HChain {
    val nil: HEmpty = Empty
    def one[A](x: A): HSingleton[A] = Singleton(x)
  }
}
