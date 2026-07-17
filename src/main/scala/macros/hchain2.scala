package experiments.macros

import experiments.macros.hlist.{HCons, HList, HNil, Tidy}

object hchain2 {

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

  type ToHList[A <: HChain] = Build[A, HNil]
  type Build[A <: HChain, Acc <: HList] <: HList = A match {
    case HEmpty => Acc
    case HSingleton[a] => HCons[a, Acc]
    case HAppend[a, b] => Build[a & HNonEmpty, Build[b & HNonEmpty, Acc]]
  }

  sealed trait HChain

  sealed trait HEmpty extends HChain
  private case object Empty extends HEmpty

  sealed trait HNonEmpty extends HChain

  sealed trait HSingleton[+A] extends HNonEmpty
  private case class Singleton[+A](x: A) extends HSingleton[A]

  sealed trait HAppend[+A, +B] extends HNonEmpty
  private case class Append[+A <: HNonEmpty, +B <: HNonEmpty](left: A, right: B) extends HAppend[A, B]

  extension [A <: HChain] (xs: A) {
    def +:[B](x: B): HPrepended[B, A] = xs match {
      case _: HEmpty             => Singleton(x)
      case nonEmptyXs: HNonEmpty => Append(Singleton(x), nonEmptyXs)
    }

    def ++[B <: HChain](ys: B): HConcat[A, B] = xs match {
      case _: HEmpty             => ys
      case nonEmptyXs: HNonEmpty => ys match {
        case _: HEmpty             => nonEmptyXs
        case nonEmptyYs: HNonEmpty => Append(nonEmptyXs, nonEmptyYs)
      }
    }

    def toHList: ToHList[A] = {
      // TODO: Make tail-recursive. Need HList of HChain accumulator
      def build[A <: HChain, Acc <: HList](xs: A, acc: Acc): Build[A, Acc] = xs match {
        case _: HEmpty => acc
        case hSingleton: HSingleton[_] => hSingleton match {
          case Singleton(a) => a +: acc
        }
        case hAppend: HAppend[_, _] => hAppend match {
          case Append(a, b) => build(a, build(b, acc))
        }
      }
      build(xs, HNil)
    }

    def tidy: Tidy[ToHList[A]] = toHList.tidy
  }

  object HChain {
    val nil: HEmpty = Empty
    def one[A](x: A): HSingleton[A] = Singleton(x)
  }
}
