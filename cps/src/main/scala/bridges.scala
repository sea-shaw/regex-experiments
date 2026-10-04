package experiments.cps

import experiments.cps.regex.*
import experiments.cps.tidy.*
import parsley.templates.{PureParserBridge1, PureParserBridge2}

object bridges {
  object Lit extends PureParserBridge1[Char, Lit] {
    override def apply(c: Char): Lit = new Lit(c)
  }

  object Cat extends PureParserBridge1[List[Reg[? <: Tuple]], Reg[? <: Tuple]] {
    override def apply(regs: List[Reg[? <: Tuple]]): Reg[? <: Tuple] = regs match {
      case reg :: Nil => reg
      case _          => new Cat(regs.foldRight[RList[? <: Tuple]](RNil)(_ +: _))
    }
  }

  object Cap extends PureParserBridge1[Reg[? <: Tuple], Reg[? <: Tuple]] {
    override def apply(reg: Reg[? <: Tuple]): Reg[? <: Tuple] = {
      /* Need a type parameter for flow-typing. */
      def cap[A <: Tuple](reg: Reg[A]): Reg[?] = reg.tag match {
        case EmptyTag      => new CapEmpty(reg)
        case NonEmptyTag() => new CapNonEmpty(reg)
      }

      cap(reg)
    }
  }

  object Opt extends PureParserBridge1[Reg[? <: Tuple], Opt[? <: Tuple]] {
    override def apply(reg: Reg[? <: Tuple]): Opt[? <: Tuple] = new Opt(reg)
  }

  object Alt extends PureParserBridge2[Reg[? <: Tuple], Reg[? <: Tuple], Alt[? <: Tuple, ? <: Tuple]] {
    override def apply(left: Reg[? <: Tuple], right: Reg[? <: Tuple]): Alt[? <: Tuple, ? <: Tuple] = new Alt(left, right)
  }
}
