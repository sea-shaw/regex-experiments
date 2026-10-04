package experiments.cps.runtime

object hlist {
  sealed trait HList[T <: Tuple]
  case object HNil extends HList[EmptyTuple]
  case class HCons[H, T <: Tuple](head: H, tail: HList[T]) extends HList[H *: T]
}
