package experiments.macros

import scala.annotation.tailrec

object hlist {

  type Concat[A <: HList, B <: HList] <: HList = A match {
    case HNil        => B
    case HCons[h, t] => HCons[h, Concat[t, B]]
  }

  type Reverse[A <: HList] = ReverseOnto[A, HNil]
  type ReverseOnto[A <: HList, B <: HList] <: HList = A match {
    case HNil        => B
    case HCons[h, t] => ReverseOnto[t, HCons[h, B]]
  }

  type Tidy[A <: HList] = A match {
    case HNil           => Unit
    case HCons[a, b] => b match {
      case HNil => a
      case _    => ToTuple[A]
    }
  }

  type ToTuple[H <: HList] <: Tuple = H match {
    case HNil        => EmptyTuple
    case HCons[h, t] => h *: ToTuple[t]
  }

  sealed trait HList {
    def ::[H](head: H) = new HCons[H, this.type](head, this)
  }

  type HNil = HNil.type
  case object HNil extends HList

  case class HCons[+H, +T <: HList](head: H, tail: T) extends HList

  extension [A <: HList] (xs: A) {
    def ++[B <: HList](ys: B): Concat[A, B] = xs match {
      case _: HNil            => ys
      case hcons: HCons[_, _] => HCons(hcons.head, (hcons.tail ++ ys))
    }

    def reverse: Reverse[A] = xs.reverseOnto(HNil)
    @tailrec
    private def reverseOnto[B <: HList](ys: B): ReverseOnto[A, B] = xs match {
      case _: HNil            => ys
      case hcons: HCons[_, _] => hcons.tail.reverseOnto(HCons(hcons.head, ys))
    }

    def tidy: Tidy[A] = xs match {
      case _: HNil            => ()
      case hcons: HCons[_, _] => hcons.tail match {
        case _: HNil => hcons.head
        case _: Any  => xs.toTuple
      }
    }

    // Could use `*:` and a dependent function to avoid `asInstanceOf` but that
    // would be O(n^2)
    def toTuple: ToTuple[A] = Tuple.fromArray(xs.toList.toArray).asInstanceOf[ToTuple[A]]

    def toList: List[Any] = {
      @tailrec
      def go(xs: HList, acc: List[Any]): List[Any] = xs match {
        case HNil         => acc.reverse
        case HCons(x, xs) => go(xs, x :: acc)
      }

      go(xs, Nil)
    }
  }
}
