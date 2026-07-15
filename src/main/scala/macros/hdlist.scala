package experiments.macros

import experiments.macros.hlist.{Concat, HCons, HList, HNil}

object hdlist {
  sealed class HDList[A <: HList] private (private val f: [B <: HList] => B => Concat[A, B]) {
    import HDList.single

    def toHList: A = f(HNil).asInstanceOf[A] // TODO: Without `asInstanceOf``

    def +:[B](x: B): HDList[HCons[B, A]] = single(x) ++ this

    def :+[B](x: B): HDList[Concat[A, HCons[B, HNil]]] = this ++ single(x)

    def ++[B <: HList](ys: HDList[B]): HDList[Concat[A, B]] = {
      val g = ys.f
      new HDList([C <: HList] => (zs: C) => f(g(zs)).asInstanceOf[Concat[Concat[A, B], C]]) // TODO: Without `asInstanceOf`
    }
  }

  object HDList {
    def fromHList[A <: HList](xs: A): HDList[A] = new HDList([B <: HList] => (ys: B) => xs ++ ys)
    val empty: HDList[HNil] = new HDList([A <: HList] => (xs: A) => xs)
    def single[A](x: A): HDList[HCons[A, HNil]] = new HDList([B <: HList] => (ys: B) => x +: ys)
  }
}
