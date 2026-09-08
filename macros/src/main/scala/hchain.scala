package experiments.macros

/* Heterogeneous chain. See cats.data.Chain for the non-heterogeneous
   version. */
// TODO: cats copyright notice?
object hchain {
  sealed trait HChain

  /* Empty HChain. Cannot appear inside an `HAppend`. */
  type HEmpty = HEmpty.type
  case object HEmpty extends HChain

  /* Non-empty `HChain`. Used to enforce the invariant that `HAppend` cannot
     contain `HEmpty`. */
  sealed trait HNonEmpty extends HChain

  /* Singleton `HChain`. Appears at the leaves of all non-empty `HChain`s. */
  case class HSingleton[+A](value: A) extends HNonEmpty

  /* Concatenation of two non-empty `HChain`s. `Append` name comes from cats. */
  case class HAppend[+A <: HNonEmpty, +B <: HNonEmpty](left: A, right: B) extends HNonEmpty

  extension [A] (x: A) {
    def singleton: HSingleton[A] = HSingleton(x)
  }

  extension [A <: HNonEmpty] (left: A) {
    def ++[B <: HNonEmpty](right: B): HAppend[A, B] = HAppend(left, right)
  }
}
