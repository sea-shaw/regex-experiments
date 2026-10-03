package experiments.cps

object state {
  sealed trait State
  case object Accept extends State
  case class Item(c: Char, next: State) extends State
  case class Begin(i: Int, next: State) extends State
  case class End(i: Int, next: State) extends State
  case class Split(left: State, right: State) extends State
}
