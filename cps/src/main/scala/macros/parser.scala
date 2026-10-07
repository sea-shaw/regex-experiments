package experiments.cps.macros

import experiments.cps.macros.bridges.*
import experiments.cps.macros.ast.Reg
import parsley.{Parsley, Result}
import parsley.combinator.option
import parsley.expr.chain
import parsley.errors.ErrorBuilder
import parsley.syntax.character.{charLift, stringLift}
import parsley.quick.{atomic, eof, many, noneOf}
import scala.quoted.Quotes

object parser {
  def parse[Err: ErrorBuilder](s: String)(using q: Quotes): Result[Err, Reg[?]] = regex.parse(s).map(_(using q))
  private lazy val regex = expr <~ eof
  private lazy val expr: Parsley[ToReg] = chain.right1(term)(Alt from '|')
  private lazy val term = Cat(many(atom))
  private lazy val atom: Parsley[ToReg] = quantifiable <~> option(postfixOps) map {
    case (reg, None)     => reg
    case (reg, Some(op)) => op(reg)
  }
  private lazy val quantifiable = nonCapture | capture | lit
  private lazy val nonCapture = atomic("(?:") ~> expr <~ ')'
  private lazy val capture = Cap('(' ~> expr <~ ')')
  private lazy val lit = Lit(noneOf(keyChars))
  private lazy val postfixOps = Opt from '?'

  private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')
}
