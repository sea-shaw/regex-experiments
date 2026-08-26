package experiments.macros.hcollections

object hchain {
  sealed trait HChain

  type HEmpty = HEmpty.type
  case object HEmpty extends HChain
  case class HSingleton[+A](value: A) extends HChain
  case class HAppend[+A <: HChain, +B <: HChain](left: A, right: B) extends HChain // TODO: Non-empty constraint

  type HConcat[A <: HChain, B <: HChain] <: HChain = A match {
    case HEmpty    => B
    case _         => B match {
      case HEmpty    => A
      case _         => HAppend[A, B]
    }
  }

  type HCons[A, B <: HChain] <: HChain = B match {
    case HEmpty    => HSingleton[A]
    case _         => HAppend[HSingleton[A], B]
  }
}
