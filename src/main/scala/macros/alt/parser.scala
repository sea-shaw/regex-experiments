package experiments.macros.alt

import experiments.macros.alt.ast.{Dot, Regex}
import experiments.macros.alt.bridges.{Alt, Capture, Cat, Lit, NonCapture, Opt, Rep0, Rep1}
import parsley.cats.combinator.some
import parsley.expr.chain
import parsley.Parsley
import parsley.quick.{atomic, eof, noneOf}
import parsley.syntax.character.{charLift, stringLift}

object parser {

  def parse(s: String) = regex.parse(s).toEither

  private lazy val regex = expr <~ eof
  private lazy val expr: Parsley[Regex[?]] = chain.right1(term)(Alt from '|')
  private lazy val term = Cat(some(atomWithPostfix))
  private lazy val atomWithPostfix = chain.postfix(atom)(postfixOps)
  private lazy val atom = nonCapture | capture | lit | dot

  private lazy val nonCapture = NonCapture(atomic("(?:") ~> expr <~ ')')
  private lazy val capture = Capture('(' ~> expr <~ ')')
  private lazy val lit = Lit(noneOf(keyChars).map(_.toInt))
  private lazy val dot = Dot from '.'

  private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')

  private lazy val postfixOps: Parsley[Regex[?] => Regex[?]] = (Opt from '?') <|> (Rep0 from '*') <|> (Rep1 from '+')
}
