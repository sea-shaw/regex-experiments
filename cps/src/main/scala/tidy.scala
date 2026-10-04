package experiments.cps

object tidy {
  type Tidy[T <: Tuple] = T match {
    case EmptyTuple    => Unit
    case NonEmptyTuple => TidyNonEmpty[NonEmptyTuple & T]
  }

  type TidyNonEmpty[T <: NonEmptyTuple] = T match {
    case Tuple1[a] => a
    case _         => T
  }

  type TCons[H <: Tuple, T <: Tuple] <: Tuple = H match {
    case EmptyTuple    => T
    case NonEmptyTuple => TidyNonEmpty[NonEmptyTuple & H] *: T
  }

  def tcons[H <: Tuple, T <: Tuple](head: H, tail: T): TCons[H, T] = head match {
    case _: EmptyTuple => tail
    case nonEmpty: NonEmptyTuple => tidyNonEmpty(nonEmpty) *: tail
  }

  def tidy[T <: Tuple](tup: T): Tidy[T] = tup match {
    case _: EmptyTuple           => ()
    case nonEmpty: NonEmptyTuple => tidyNonEmpty(nonEmpty)
  }

  def tidyNonEmpty[T <: NonEmptyTuple](nonEmpty: T): TidyNonEmpty[T] = nonEmpty match {
    case tuple1: Tuple1[a] => tuple1._1
    case _                 => nonEmpty
  }
}
