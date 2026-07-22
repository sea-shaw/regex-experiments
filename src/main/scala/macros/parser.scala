package experiments.macros

import experiments.macros.ast.{Regex}
import experiments.macros.bridges.{Alt, Capture, Cat, Dot, Lit, NonCapture, Opt, Rep0, Rep1}
import parsley.{Parsley, Result}
import parsley.cats.combinator.some
import parsley.expr.chain
import parsley.quick.{atomic, eof, noneOf}
import parsley.syntax.character.{charLift, stringLift}
import scala.quoted.Quotes

object parser {

  def parse(s: String)(using q: Quotes): Result[String, Regex[?]] = regex.parse(s).map(_(using q))

  private lazy val regex = expr <~ eof
  private lazy val expr: Parsley[Quotes ?=> Regex[?]] = chain.right1(term)(Alt from '|')
  private lazy val term = Cat(some(atomWithPostfix))
  private lazy val atomWithPostfix = chain.postfix(atom)(postfixOps)
  private lazy val atom = nonCapture | capture | lit | dot

  private lazy val nonCapture = NonCapture(atomic("(?:") ~> expr <~ ')')
  private lazy val capture = Capture('(' ~> expr <~ ')')
  private lazy val lit = Lit(noneOf(keyChars).map(_.toInt))
  private lazy val dot = (Dot from '.')

  private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')

  private lazy val postfixOps = (Opt from '?') <|> (Rep0 from '*') <|> (Rep1 from '+')
}
