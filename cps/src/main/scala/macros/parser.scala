package experiments.cps.macros

import experiments.cps.macros.bridges.*
import parsley.{Parsley, Result}
import parsley.combinator.option
import parsley.expr.chain
import parsley.errors.ErrorBuilder
import parsley.syntax.character.charLift
import parsley.quick.{eof, many, noneOf}

object parser {
  def parse[Err: ErrorBuilder](s: String): Result[Err, ToReg] = regex.parse(s)
  private lazy val regex = expr <~ eof
  private lazy val expr: Parsley[ToReg] = chain.right1(term)(Alt from '|')
  private lazy val term = Cat(many(atom))
  private lazy val atom: Parsley[ToReg] = quantifiable <~> option(postfixOps) map {
    case (reg, None)     => reg
    case (reg, Some(op)) => op(reg)
  }
  private lazy val quantifiable = capture | lit
  private lazy val capture = Cap('(' ~> expr <~ ')')
  private lazy val lit = Lit(noneOf(keyChars))
  private lazy val postfixOps = Opt from '?'

  private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')
}
