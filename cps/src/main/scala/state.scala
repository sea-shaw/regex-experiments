package experiments.cps

import cats.data.Chain

object state {
  sealed trait State {
    final def run(s: String): Option[List[String]] = go(s, 0, Chain.nil, Map.empty)
    def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]]
  }

  case object Accept extends State {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = Some(caps.toList)
  }

  case class Item(c: Char, next: State) extends State {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = if s.charAt(i) == c then next.go(s, i + 1, caps, starts) else None
  }

  case class Begin(n: Int, next: State) extends State {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = next.go(s, i, caps, starts.updated(n, i))
  }

  case class End(n: Int, next: State) extends State {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = next.go(s, i, s.substring(starts(n), i) +: caps, starts)
  }

  case class Split(left: State, right: State) extends State {
    override def go(s: String, i: Int, caps: Chain[String], starts: Map[Int, Int]): Option[List[String]] = left.go(s, i, caps, starts) orElse right.go(s, i, caps, starts)
  }
}
