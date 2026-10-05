package experiments.cps.macros

import experiments.cps.macros.stack.*
import experiments.cps.macros.state.*
import experiments.cps.tidy.*
import scala.quoted.{Expr, Quotes, Type}
import scala.annotation.unused

object ast {
  sealed trait RList[T <: Tuple](using val tag: TupleTag[T]) {
    val numCaps: Int

    final def +:[H <: Tuple](reg: Reg[H])(using Quotes): RList[TCons[H, T]] = reg.tag match {
      case EmptyTag      => RConsEmpty(reg, this)
      case NonEmptyTag() => {
        given Type[H] = reg.tpe
        given Type[T] = tpe
        RConsNonEmpty(reg, this)
      }
    }
  
    def cps[Xs <: Tuple, R <: Tuple: Type](next: State[T *: Xs, R], i: Int)(using Quotes): State[Xs, R]
    def tpe(using Quotes): Type[T]
  }

  case object RNil extends RList[EmptyTuple] {
    override val numCaps: Int = 0

    override def cps[Xs <: Tuple, R <: Tuple: Type](next: State[EmptyTuple *: Xs, R], i: Int)(using Quotes): State[Xs, R] = {
      Output(Push(CodeTuple(QNil)), next)
    }

    override def tpe(using Quotes): Type[EmptyTuple] = Type.of
  }

  case class RConsEmpty[T <: Tuple](head: Reg[EmptyTuple], tail: RList[T]) extends RList[T](using tail.tag) {
    override val numCaps: Int = tail.numCaps

    override def cps[Xs <: Tuple, R <: Tuple: Type](next: State[T *: Xs, R], i: Int)(using Quotes): State[Xs, R] = {
      head.cps(tail.cps(next, i + head.numCaps), i)
    }

    override def tpe(using Quotes): Type[T] = tail.tpe
  }

  case class RConsNonEmpty[H <: NonEmptyTuple: Type, T <: Tuple: Type](head: Reg[H], tail: RList[T]) extends RList[TidyNonEmpty[H] *: T] {
    override val numCaps: Int = head.numCaps + tail.numCaps

    override def cps[Xs <: Tuple, R <: Tuple: Type](next: State[(TidyNonEmpty[H] *: T) *: Xs, R], i: Int)(using Quotes): State[Xs, R] = {
      head.cps(tail.cps(Output(Reduce(qTcons), next), i + head.numCaps), i)
    }

    override def tpe(using Quotes): Type[TidyNonEmpty[H] *: T] = Type.of
  }

  sealed trait Reg[A <: Tuple](using val tag: TupleTag[A]) {
    val numCaps: Int

    final def compile(using Quotes, Type[A]): State[EmptyTuple, A] = cps(Accept(), 0)

    def cps[T <: Tuple, R <: Tuple: Type](next: State[TCons[A, T], R], i: Int)(using Quotes): State[T, R]

    def tpe(using Quotes): Type[A]
  }

  case class Lit(c: Char) extends Reg[EmptyTuple] {
    override val numCaps: Int = 0

    override def cps[T <: Tuple, R <: Tuple: Type](next: State[TCons[EmptyTuple, T], R], i: Int)(using Quotes): State[T, R] = {
      Item(c, next)
    }

    override def tpe(using Quotes): Type[EmptyTuple] = Type.of
  }

  case class Cat[A <: Tuple: Type] private (regs: RList[A]) extends Reg[A](using regs.tag) {
    override val numCaps: Int = regs.numCaps

    override def cps[T <: Tuple, R <: Tuple: Type](next: State[TCons[A, T], R], i: Int)(using Quotes): State[T, R] = {
      tag match {
        case EmptyTag      => regs.cps(Output(Drop(), next), i)
        case NonEmptyTag() => regs.cps(Output(Apply(qTidyNonEmpty), next), i) // TODO: Lift `tidy`
      }
    }

    override def tpe(using Quotes): Type[A] = regs.tpe
  }

  object Cat {
    def apply[A <: Tuple](regs: RList[A])(using Quotes): Cat[A] = {
      given Type[A] = regs.tpe
      new Cat(regs)
    }
  }

  case class CapEmpty(reg: Reg[EmptyTuple]) extends Reg[Tuple1[String]] {
    override val numCaps: Int = 1

    override def cps[T <: Tuple, R <: Tuple: Type](next: State[String *: T, R], i: Int)(using Quotes): State[T, R] = {
      Begin(i, reg.cps(End(i, next), i + 1))
    }

    override def tpe(using Quotes): Type[Tuple1[String]] = Type.of
  }

  case class CapNonEmpty[A <: NonEmptyTuple: Type] private (reg: Reg[A]) extends Reg[Tuple2[String, TidyNonEmpty[A]]] {
    override val numCaps: Int = 1 + reg.numCaps

    override def cps[T <: Tuple, R <: Tuple: Type](next: State[TCons[(String, TidyNonEmpty[A]), T], R], i: Int)(using Quotes): State[T, R] = {
      Begin(i, reg.cps(End(i, Output(Reduce((inner, cap) => CodeExpr('{ (${ cap.toExpr }, $ {inner.toExpr }) })), next)), i + 1))
    }

    override def tpe(using Quotes): Type[(String, TidyNonEmpty[A])] = Type.of
  }

  object CapNonEmpty {
    def apply[A <: NonEmptyTuple](reg: Reg[A])(using Quotes): CapNonEmpty[A] = {
      given Type[A] = reg.tpe
      new CapNonEmpty(reg)
    }
  }

  case class Opt[A <: Tuple: Type] private (reg: Reg[A]) extends Reg[Tuple1[Option[Tidy[A]]]] {
    override val numCaps: Int = reg.numCaps

    override def cps[T <: Tuple, R <: Tuple: Type](next: State[TCons[Tuple1[Option[Tidy[A]]], T], R], i: Int)(using Quotes): State[T, R] = {
      val someOp = op(reg.tag, x => '{ Some($x) }, next)
      Split(reg.cps(Output(someOp, next), i), Output(Push(CodeExpr('{ None })), next))
    }

    override def tpe(using Quotes): Type[Tuple1[Option[Tidy[A]]]] = Type.of
  }

  object Opt {
    def apply[A <: Tuple](reg: Reg[A])(using Quotes): Opt[A] = {
      given Type[A] = reg.tpe
      new Opt(reg)
    }
  }

  case class Alt[A <: Tuple: Type, B <: Tuple: Type] private (left: Reg[A], right: Reg[B]) extends Reg[Tuple1[Either[Tidy[A], Tidy[B]]]] {
    override val numCaps: Int = left.numCaps + right.numCaps

    override def cps[T <: Tuple, R <: Tuple: Type](next: State[TCons[Tuple1[Either[Tidy[A], Tidy[B]]], T], R], i: Int)(using Quotes): State[T, R] = {
      val leftOp = op(left.tag, x => '{ Left($x) }, next)
      val rightOp = op(right.tag, x => '{ Right($x) }, next)
      Split(left.cps(Output(leftOp, next), i), right.cps(Output(rightOp, next), i + left.numCaps))
    }

    override def tpe(using Quotes): Type[Tuple1[Either[Tidy[A], Tidy[B]]]] = Type.of
  }

  object Alt {
    def apply[A <: Tuple, B <: Tuple](left: Reg[A], right: Reg[B])(using Quotes): Alt[A, B] = {
      given Type[A] = left.tpe
      given Type[B] = right.tpe
      new Alt(left, right)
    }
  }

  private def op[A <: Tuple, B: Type, T <: Tuple](tag: TupleTag[A], f: Expr[Tidy[A]] => Quotes ?=> Expr[B], @unused next: State[B *: T, ?])(using Quotes): Op[TCons[A, T], B *: T] = tag match {
    case EmptyTag      => Push(CodeExpr(f('{ () })))
    case NonEmptyTag() => Apply(expr => CodeExpr(f(expr.toExpr)))
  }
}
