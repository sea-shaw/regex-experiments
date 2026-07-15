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
    def +:[H, T >: this.type <: HList](head: H) = new HCons[H, T](head, this)
  }

  type HNil = HNil.type
  case object HNil extends HList

  case class HCons[+H, +T <: HList] private [hlist] (head: H, tail: T) extends HList

  given [A <: HList] => Conversion[Concat[A, HNil], A] {
    override def apply(hlist: Concat[A, HNil]): A = hlist.asInstanceOf[A]
  }

  given [A <: HList, B <: HList, C <: HList] => Conversion[Concat[A, Concat[B, C]], Concat[Concat[A, B], C]] {
    override def apply(hlist: Concat[A, Concat[B, C]]): Concat[Concat[A, B], C] = hlist.asInstanceOf[Concat[Concat[A, B], C]]
  }

  extension [A <: HList] (xs: A) {
    def ++[B <: HList](ys: B): Concat[A, B] = xs match {
      case _: HNil            => ys
      case hcons: HCons[_, _] => hcons.head +: (hcons.tail ++ ys)
    }

    def reverse: Reverse[A] = xs.reverseOnto(HNil)
    @tailrec
    private def reverseOnto[B <: HList](ys: B): ReverseOnto[A, B] = xs match {
      case _: HNil            => ys
      case hcons: HCons[_, _] => hcons.tail.reverseOnto(hcons.head +: ys)
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
