package experiments.cps.macros

import experiments.cps.macros.ast
import experiments.cps.macros.ast.{Reg, RList, RNil}
import experiments.cps.macros.config.Config
import experiments.cps.tidy.*
import parsley.templates.{PureParserBridge1, PureParserBridge2}
import scala.quoted.Quotes

object bridges {
  type ToReg = (Config, Quotes) ?=> Reg[? <: Tuple]

  object Lit extends PureParserBridge1[Char, ToReg] {
    override def apply(c: Char): ToReg = ast.Lit(c)
  }

  object Cat extends PureParserBridge1[List[ToReg], ToReg] {
    override def apply(regs: List[ToReg]): ToReg = regs match {
      case reg :: Nil => reg
      case _          => ast.Cat(regs.foldRight[RList[?]](RNil)(_ +: _))
    }
  }

  object Cap extends PureParserBridge1[ToReg, ToReg] {
    override def apply(reg: ToReg): ToReg = {
      /* Need a type parameter for flow-typing. */
      def cap[A <: Tuple](reg: Reg[A]): Reg[?] = reg.tag match {
        case EmptyTag      => ast.CapEmpty(reg)
        case NonEmptyTag() => ast.CapNonEmpty(reg)
      }

      cap(reg)
    }
  }

  object Opt extends PureParserBridge1[ToReg, ToReg] {
    override def apply(reg: ToReg): ToReg = {
      def opt[A <: Tuple](reg: Reg[A]): ToReg = {
        reg.tag match {
          case EmptyTag if ignoreEmpty => ast.OptEmpty(reg)
          case _                       => ast.Opt(reg) 
        }
      }
      opt(reg)
    }
  }

  object Alt extends PureParserBridge2[ToReg, ToReg, ToReg] {
    override def apply(left: ToReg, right: ToReg): ToReg = {
      def alt[A <: Tuple, B <: Tuple](left: Reg[A], right: Reg[B]): ToReg = {
        (left.tag, right.tag) match {
          case (EmptyTag, EmptyTag) if ignoreEmpty => ast.AltEmpty(left, right)
          case _                                   => ast.Alt(left, right)
        }
      }
      alt(left, right)
    }
  }

  object Rep0 extends PureParserBridge1[ToReg, ToReg] {
    override def apply(reg: ToReg): ToReg = ast.Rep0(reg)
  }

  object PositiveLookahead extends PureParserBridge1[ToReg, ToReg] {
    override def apply(reg: ToReg): ToReg = ast.PositiveLookahead(reg)
  }

  def ignoreEmpty(using config: Config): Boolean = config.ignoreEmpty
}
