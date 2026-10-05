package experiments.cps.macros

import experiments.cps.macros.regex
import experiments.cps.macros.regex.{Reg, RList, RNil}
import experiments.cps.tidy.*
import parsley.templates.{PureParserBridge1, PureParserBridge2}
import scala.quoted.Quotes

object bridges {
  type ToReg = Quotes ?=> Reg[? <: Tuple]

  object Lit extends PureParserBridge1[Char, ToReg] {
    override def apply(c: Char): ToReg = regex.Lit(c)
  }

  object Cat extends PureParserBridge1[List[ToReg], ToReg] {
    override def apply(regs: List[ToReg]): ToReg = regs match {
      case reg :: Nil => reg
      case _          => regex.Cat(regs.foldRight[RList[? <: Tuple]](RNil)(_ +: _))
    }
  }

  object Cap extends PureParserBridge1[ToReg, ToReg] {
    override def apply(reg: ToReg): ToReg = {
      /* Need a type parameter for flow-typing. */
      def cap[A <: Tuple](reg: Reg[A]): Reg[?] = reg.tag match {
        case EmptyTag      => regex.CapEmpty(reg)
        case NonEmptyTag() => regex.CapNonEmpty(reg)
      }

      cap(reg)
    }
  }

  object Opt extends PureParserBridge1[ToReg, ToReg] {
    override def apply(reg: ToReg): ToReg = regex.Opt(reg)
  }

  object Alt extends PureParserBridge2[ToReg, ToReg, ToReg] {
    override def apply(left: ToReg, right: ToReg): ToReg = regex.Alt(left, right)
  }
}
