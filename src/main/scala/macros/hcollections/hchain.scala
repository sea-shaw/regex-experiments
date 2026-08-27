package experiments.macros.hcollections

object hchain {
  sealed trait HChain

  type HEmpty = HEmpty.type
  case object HEmpty extends HChain

  sealed trait HNonEmpty extends HChain

  case class HSingleton[+A](value: A) extends HNonEmpty
  case class HAppend[+A <: HNonEmpty, +B <: HNonEmpty](left: A, right: B) extends HNonEmpty
}
