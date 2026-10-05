package experiments.cps.macros

import scala.quoted.Expr

object qlist {
  sealed trait QList[T <: Tuple]
  case object QNil extends QList[EmptyTuple]
  case class QCons[H, T <: Tuple](head: Expr[H], tail: QList[T]) extends QList[H *: T]
}
