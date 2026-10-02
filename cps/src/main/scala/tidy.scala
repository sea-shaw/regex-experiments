package experiments.cps

object tidy {
  type Tidy[T <: Tuple] = T match {
    case EmptyTuple    => Unit
    case NonEmptyTuple => TidyNonEmpty[T]
  }
  type TidyNonEmpty[T <: NonEmptyTuple] = T match {
    case Tuple1[a] => a
    case _         => T
  }

}
