package experiments.macros

import experiments.macros.hlist.{Concat, HCons, HList, HNil}

object hchain {

  sealed trait HChain[A <: HList] {
    def ++[B <: HList](bs: HChain[B]): HChain[Concat[A, B]]
    def +:[B](b: B): HChain[HCons[B, A]] = HChain.one(b) ++ this
    def toHList: A = build(HNil).asInstanceOf[A]
    private [hchain] def build[B <: HList](acc: B): Concat[A, B]
  }

  private case object HEmpty extends HChain[HNil] {
    override def ++[B <: HList](bs: HChain[B]): HChain[B] = bs
    override private [hchain] def build[B <: HList](acc: B): B = acc
  }

  private sealed trait NonEmptyHChain[A <: HList] extends HChain[A] {
    override def ++[B <: HList](bs: HChain[B]): HChain[Concat[A, B]] = bs match {
      case nonEmptyBs: NonEmptyHChain[B] => HAppend(this, nonEmptyBs)
      case HEmpty                        => this.asInstanceOf[HChain[Concat[A, B]]]
    }
  }

  private case class HSingleton[A](a: A) extends NonEmptyHChain[HCons[A, HNil]] {
    override private [hchain] def build[B <: HList](acc: B): HCons[A, B] = HCons(a, acc)
  }

  private case class HAppend[A <: HList, B <: HList](as: NonEmptyHChain[A], bs: NonEmptyHChain[B]) extends NonEmptyHChain[Concat[A, B]] {
    override private [hchain] def build[C <: HList](acc: C): Concat[Concat[A, B], C] = {
      as.build(bs.build(acc)).asInstanceOf[Concat[Concat[A, B], C]]
    }
  }

  object HChain {
    def one[A](a: A): HChain[HCons[A, HNil]] = HSingleton(a)
    val nil: HChain[HNil] = HEmpty
  }
}
