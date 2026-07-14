package experiments.macros

import experiments.macros.ast.{Dot, Regex}
import experiments.macros.bridges.{Alt, Capture, Cat, Lit, NonCapture, Opt, Rep0, Rep1}
import parsley.expr.chain
import parsley.Parsley
import parsley.quick.{atomic, eof, noneOf, many}
import parsley.syntax.character.{charLift, stringLift}

object parser {

  def parse(s: String) = regex.parse(s).toEither

  lazy val regex = expr <~ eof
  private lazy val expr: Parsley[Regex[?]] = chain.right1(term)(Alt from '|')
  private lazy val term = Cat(atomWithPostfix, many(atomWithPostfix)) // TODO: use NonEmptyList from parsley cats, currently incompatible
  private lazy val atomWithPostfix = chain.postfix(atom)(postfixOps)
  private lazy val atom = nonCapture | capture | lit | dot

  private lazy val nonCapture = NonCapture(atomic("(?:") ~> expr <~ ')')
  private lazy val capture = Capture('(' ~> expr <~ ')')
  private lazy val lit = Lit(noneOf(keyChars).map(_.toInt))
  private lazy val dot = Dot from '.'

  private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')

  private lazy val postfixOps: Parsley[Regex[?] => Regex[?]] = (Opt from '?') <|> (Rep0 from '*') <|> (Rep1 from '+')
}
