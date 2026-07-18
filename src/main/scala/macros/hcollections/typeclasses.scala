package experiments.macros.hcollections

object tpyeclasses {
  trait HSemigroup[H] {
    type Combine[_ <: H, _ <: H] <: H

    def combine[A <: H, B <: H](x: A, y: B): Combine[A, B]
  }

  trait HMonoid[H] extends HSemigroup[H] {
    type Empty <: H

    def empty: Empty
  }
}
