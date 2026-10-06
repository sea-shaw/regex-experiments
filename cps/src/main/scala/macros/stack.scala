package experiments.cps.macros

import scala.quoted.{Expr, Type, Quotes}
import experiments.cps.tidy.*

object stack {
  sealed trait Stack[T <: Tuple]
  case object SNil extends Stack[EmptyTuple]
  case class SCons[H, T <: Tuple](head: Code[H], tail: Stack[T]) extends Stack[H *: T]

  sealed trait Code[A] {
    def toExpr(using Quotes): Expr[A]
  }

  case class CodeExpr[A](expr: Expr[A]) extends Code[A] {
    override def toExpr(using Quotes): Expr[A] = expr
  }

  case class CodeTuple[T <: Tuple](elems: QList[T]) extends Code[T] {
    override def toExpr(using Quotes): Expr[T] = elems.toExpr
  }

  sealed trait QList[T <: Tuple] {
    def tidyCode(using Quotes): Code[Tidy[T]]
  }

  case object QNil extends QList[EmptyTuple] {
    override def tidyCode(using Quotes): Code[Unit] = CodeExpr('{ () })
  }

  case class QCons[H, T <: Tuple](expr: Expr[H], tpe: Type[H], tail: QList[T]) extends QList[H *: T] {
    override def tidyCode(using Quotes): Code[TidyNonEmpty[H *: T]] = {
      tail match {
        case QNil           => CodeExpr(expr)
        case QCons(_, _, _) => CodeTuple(this)
      }
    }
  }

  extension [T <: Tuple] (qlist: QList[T]) {
    def toExpr(using Quotes): Expr[T] = qlist match {
      case QNil => '{ EmptyTuple }
      case QCons(e0, given Type[t0], tail0) => tail0 match {
        case QNil => '{ Tuple1($e0) }
        case QCons(e1, given Type[t1], tail1) => tail1 match {
          case QNil => '{ Tuple2($e0, $e1) }
          case QCons(e2, given Type[t2], tail2) => tail2 match {
            case QNil => '{ Tuple3($e0, $e1, $e2) }
            case QCons(e3, given Type[t3], tail3) => tail3 match {
              case QNil => '{ Tuple4($e0, $e1, $e2, $e3) }
              case _    => ???
            }
          }
        }
      }
    }
  }

  def codeCons[H: Type as tpe, T <: Tuple: Type](head: Code[H], tail: Code[T])(using Quotes): Code[H *: T] = tail match {
    case CodeExpr(tailExpr) => CodeExpr('{ ${ head.toExpr } *: $tailExpr })
    case CodeTuple(elems)   => CodeTuple(QCons(head.toExpr, tpe, elems))
  }

  def codeTidyNonEmpty[T <: NonEmptyTuple: Type](tuple: Code[T])(using Quotes): Code[TidyNonEmpty[T]] = tuple match {
    case CodeExpr(expr)   => CodeExpr('{ tidyNonEmpty($expr) })
    case CodeTuple(elems) => elems.tidyCode
  }
}
