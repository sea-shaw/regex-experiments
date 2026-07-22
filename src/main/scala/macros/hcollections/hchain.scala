package experiments.macros.hcollections

import scala.annotation.tailrec

object hchain {

  type HConcat[A <: HChain, B <: HChain] = A match {
    case HEmpty    => B
    case HNonEmpty => B match {
      case HEmpty    => A
      case HNonEmpty => HAppend[A, B]
    }
  }

  type HCons[A, B <: HChain] = B match {
    case HEmpty    => HSingleton[A]
    case HNonEmpty => HAppend[HSingleton[A], B]
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
    case HAppend[a, b] => Build[a, Build[b, Acc]]
  }

  sealed trait HChain {
    def +:[A, B >: this.type <: HChain](x: A): HCons[A, B] = (this: B) match {
      case _: HEmpty             => Singleton(x)
      case nonEmptyXs: HNonEmpty => Append(Singleton(x), nonEmptyXs)
    }
  }

  sealed trait HEmpty extends HChain
  private case object Empty extends HEmpty

  sealed trait HNonEmpty extends HChain

  sealed trait HSingleton[+A] extends HNonEmpty {
    val value: A
  }
  private case class Singleton[+A](value: A) extends HSingleton[A]

  sealed trait HAppend[+A, +B] extends HNonEmpty {
    val left: A
    val right: B
  }
  private case class Append[+A <: HNonEmpty, +B <: HNonEmpty](left: A, right: B) extends HAppend[A, B]

  extension [A <: HChain] (xs: A) {
    def ++[B <: HChain](ys: B): HConcat[A, B] = xs match {
      case _: HEmpty             => ys
      case nonEmptyXs: HNonEmpty => ys match {
        case _: HEmpty             => nonEmptyXs
        case nonEmptyYs: HNonEmpty => Append(nonEmptyXs, nonEmptyYs)
      }
    }

    def toList: List[Any] = {
      @tailrec
      def go(xs: HChain, acc: List[Any], rest: List[HNonEmpty]): List[Any] = xs match {
        case Empty => rest match {
          case Nil => acc
          case head :: tail => go(head, acc, tail)
        }
        case Singleton(x) => go(Empty, x :: acc, rest)
        case Append(left, right) => go(right, acc, left :: rest)
      }

      go(xs, Nil, Nil)
    }

    // TODO: `ArrayBuilder`? Don't want to use `*:` because it's O(n^2) overall
    def tidy: Tidy[A] = {
      val tup = Tuple.fromArray(toList.toArray).asInstanceOf[ToTuple[A]]
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
